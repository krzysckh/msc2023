;; (define (funkcja a b c d)
;;   "ta funkcja robi bla bla bla"
;;   (example
;;    '((funkcja 10 20 30 40) 'costam)
;;    '((funkcja 1 2 3 4) 'inne-costam))
;;   (args
;;    '((a . "robi costam")
;;      (b . "odpowiedzialne za innestam"))))

;; TODO: escape-owanie znaków w tekstach
;; TODO: może być tylko jeden przykład na funkcje lol lol lol

;; https://github.com/lispunion/code-formatter
(define scheme-format-path "scheme-format")

(define scm-files
  (filter
   (lambda (v) (string=? ".scm" (substring v (- (string-length v) 4) (string-length v))))
   (split-string (sys '(ls scm)) "\n")))

(define real-filenames
  (map (lambda (s) (string-append "scm/" s)) scm-files)) ;; '("tinyscheme/r5rs.scm")))

(define (caar* l)
  (if (pair? l)
      (if (pair? (car l))
          (caar l)
          '())
      '()))

(define (find-thing l thing)
  (cond
   ((null? l) '())
   ((eqv? (caar* l) thing) (cdar l))
   (else
     (find-thing (cdr l) thing))))

(define (find-examples l) (find-thing l 'example))
(define (find-args-docs l) (find-thing l 'args))

(define (render-function f)
  (let ((name (cadr (assq 'f f)))
        (args (cadr (assq 'args f)))
        (doc (cadr (assq 'docstring f)))
        (examples (cadr (assq 'examples f)))
        (args-docs (cadr (assq 'args-docs f)))
        (impl (cadr (assq 'impl f))))
    (pprint "### " (append (list name) args))
    (pprint doc "\n")
    (when (> (length (flatten args)) 0)
      (print "*argumenty*\n"))
    (for-each
     (lambda (a)
       (pprint
        "- " a
        (if (assq a args-docs)
            (string-append " - " (cdr (assq a args-docs)))
            "")))
     (flatten args))
    (when (> (length examples) 0)
      (print "\n\n*przykłady*\n"))
    (for-each
     (lambda (ex)
       (pprint "`"
               (map (lambda (v)
                      (if (string? v)
                          (string-append "\"" v "\"")
                          v))
                    (car ex)) "`" " → " "*" (cadr ex) "*"))
     examples)
    (print "")
    (print "<details> <summary> implementacja </summary>\n\n")
    (with-output-to-file "/tmp/gen"
      (→ (write impl)))
    (print "```scheme")
    (print (sys (list scheme-format-path "/tmp/gen")))
    (print "```")
    (print "</details>\n")))

(define (render d filename)
  (pprint
   "## [" filename
   "](https://git.krzysckh.org/kpm/science-cup-2023/src/branch/master/"
   filename ")\n")
  (print "<details> <summary> definiowane funkcje </summary>\n\n")
  (map render-function d)
  (print "</details>\n"))

(define (render-file name)
  (let* ((sexps (call-with-input-file name (lambda (f) (serialize:read-sexps f '()))))
         (functions
          (filter
           (lambda (v) (and (or (eqv? (car v) 'define)
                           (eqv? (car v) 'document-function))
                       (or (list? (cadr v))
                           (pair? (cadr v)))))
           sexps))
         (docs
          (map
           (lambda (func)
             (let ((docstring (if (string? (caddr func)) (caddr func) ""))
                   (args-list (cadr func))
                   (args-docs (find-args-docs func))
                   (examples (find-examples func)))
               `((f ,(car args-list))
                 (impl ,func)
                 (docstring ,docstring)
                 (args ,(cdr args-list))
                 (examples ,(if (null? examples) '() (cdar examples)))
                 (args-docs ,(if (null? args-docs) '() (cadar args-docs))))))
           functions)))
    (render docs name)))

(map render-file real-filenames)
