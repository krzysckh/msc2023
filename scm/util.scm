(macro (example . v) '())
(macro (args . v) '())

(define (print s . l)
  "wypisuje argumenty do konsoli"
  (for-each (lambda (v) (display v) (display " ")) (append (list s) l))
  (newline))

(define (pprint s . l)
  "wypisuje argumenty do konsoli, bez spacji poiędzy"
  (for-each display (append (list s) l))
  (newline))

(define (max2 a b) "zwraca większą wartość pomiędzy a, a b" (if (> a b) a b))
(define (min2 a b) "zwraca mniejszą wartość pomiędzy a, a b" (if (< a b) a b))

(define (max . ns)
  "zwraca największą wartość pośród argumentów"
  (example
   '((max 1 2 3) 3))
  (foldr max2 (car ns) ns))

(define (min . ns)
  "zwraca najmniejszą wartość pośród argumentów"
  (example
   '((min 1 2 3) 1))
  (foldr min2 (car ns) ns))

(define (maxl lst)
  "zwraca największą wartość z listy"
  (example
   '((max '(1 2 3)) 3))
  (apply max lst))

(define (minl lst)
  "zwraca najmniejszą wartość z listy"
  (example
   '((min '(1 2 3)) 1))
  (apply min lst))

;; bezczelnie ukradzione z https://github.com/krzysckh/robusta
;; (od samego siebie)
(define (bool->string v) (if v "#t" "#f"))
(define (->string x)
  (cond
   ((list? x) (foldr string-append
                     ""
                     (map (lambda (x) (string-append (->string x) " ")) x)))
   ((pair? x) (string-append (->string (car x)) " "
                             (->string (cdr x)) " "))
   ((number? x) (number->string x))
   ((symbol? x) (symbol->string x))
   ((boolean? x) (bool->string x))
   ((char? x) (string x))
   ((string? x) x)
   (else
    "???")))

(define (->char x)
  (cond
   ((number? x) (->char (number->string x)))
   ((string? x) (car (string->list x)))
   ((char? x) x)
   (else
    (error "->char: unexpected type"))))

(define (string-split str c)
  (let ((end (string-length str)) (ch (->char c)))
    (let lp ((from 0) (to 0) (res '()))
      (cond
       ((>= to end)
        (reverse (if (> to from) (cons (substring str from to) res) res)))
       ((eqv? ch (string-ref str to))
        (lp (+ to 1) (+ to 1) (cons (substring str from to) res)))
       (else
        (lp from (+ to 1) res))))))
(define split-string string-split)

; https://code.whatever.social/questions/9458982/how-is-filter-implemented#9459181
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else
         (filter pred (cdr lst)))))

; https://code.whatever.social/questions/8387583/writing-flatten-method-in-scheme#58914237
(define (flatten lst)
  (let loop ((lst lst) (acc '()))
    (cond
     ((null? lst) acc)
     ((pair? lst) (loop (car lst) (loop (cdr lst) acc)))
     (else
      (cons lst acc)))))

(define (sys . l)
  "uruchamia `system`, wcześniej zamieniając argumenty na jeden string"
  (system (apply string-append (map (lambda (v) (string-append (->string v) " ")) l))))

(define (iota s step e)
  (letrec ((I (lambda (s step e acc)
                (cond
                  ((>= s e) acc)
                  (else
                    (I (+ s step) step e (append acc (list s))))))))
    (I s step e '())))

(define (sum l)
  (apply + l))

; pt = (x . y), rect = (x y w h)
(define (point-in-rect? pt rect)
  (let ((px (car pt))
        (py (cdr pt))
        (rx (list-ref rect 0))
        (ry (list-ref rect 1))
        (rw (list-ref rect 2))
        (rh (list-ref rect 3)))
    (and (>= px rx) (<= px (+ rx rw)) (>= py ry) (<= py (+ ry rh)))))

(define (split-every lst n)
  (letrec ((f (lambda (in ret acc)
                (cond
                  ((null? in) (if (null? acc) ret (append ret (list acc))))
                  ((eqv? (length acc) n) (f in (append ret (list acc)) '()))
                  (else
                    (f (cdr in) ret (append acc (list (car in)))))))))
    (f lst '() '())))
