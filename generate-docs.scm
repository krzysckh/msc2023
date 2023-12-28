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

(define scm-files
  (filter
   (lambda (v) (string=? ".scm" (substring v (- (string-length v) 4) (string-length v))))
   (split-string (sys '(ls scm)) "\n")))

(define real-filenames (map (lambda (s) (string-append "scm/" s)) scm-files))

(define (read-sexps f acc)
  (let ((sexp (read f)))
    (cond
     ((eof-object? sexp) acc)
     (else
      (read-sexps f (append acc (list sexp)))))))

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
        (args-docs (cadr (assq 'args-docs f))))
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
      (print "*przykłady*\n"))
    (for-each
     (lambda (ex)
       (pprint "`" (car ex) "`" " → " "*" (cadr ex) "*")
       )
     examples)
    (print "")))

(define (render d filename)
  (pprint
   "## [" filename
   "](https://git.krzysckh.org/kpm/science-cup-2023/src/branch/master/"
   filename ")\n")
  (map render-function d))

(define (render-file name)
  (define sexps (call-with-input-file name (lambda (f) (read-sexps f '()))))
  (define functions
    (filter
     (lambda (v) (and (eqv? (car v) 'define)
                 (or (list? (cadr v))
                     (pair? (cadr v)))))
     sexps))
  (define docs
    (map
     (lambda (func)
       (let ((docstring (if (string? (caddr func)) (caddr func) ""))
             (args-list (cadr func))
             (args-docs (find-args-docs func))
             (examples (find-examples func)))
         `((f ,(car args-list))
           (docstring ,docstring)
           (args ,(cdr args-list))
           (examples ,(if (null? examples) '() (cdar examples)))
           (args-docs ,(if (null? args-docs) '() (cadar args-docs))))))
     functions))
  (render docs name))

(map render-file real-filenames)
