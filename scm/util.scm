(macro (example . v) '())
(macro (args . v) '())
(macro nil '())
(macro t #t)
(macro f #f)

;; "programowanie" ""funkcyjne"" :33333
(macro (-- v) `(set! ,(cadr v) (- ,(cadr v) 1)))
(macro (++ v) `(set! ,(cadr v) (+ ,(cadr v) 1)))
(macro add1 ++)
(macro sub1 --)

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
  "zamienia cokolwiek na string"
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
  "zamienia cokolwiek na znak"
  (cond
   ((number? x) (->char (number->string x)))
   ((string? x) (car (string->list x)))
   ((char? x) x)
   (else
    (error "->char: unexpected type"))))

(define (string-split str c)
  "tnie *str* na każdym napodkanym *c*"
  (example
   '((string-split "abc|def|ghi" "|") ("abc" "def" "ghi")))

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
(define (filter f lst)
  "wywołuje f dla każdego elementu z lst i zwraca listę elementów w których f zwraca #t (lub inną nie-#f wartość)"
  (example
   '((filter (lambda (v) (eq? 1 v)) '(1 2 3 1 2 5)) (1 1)))
  (cond ((null? lst) '())
        ((f (car lst)) (cons (car lst) (filter f (cdr lst))))
        (else
         (filter f (cdr lst)))))

; https://code.whatever.social/questions/8387583/writing-flatten-method-in-scheme#58914237
(define (flatten lst)
  "zamienia zagnieżdżone listy w lst na jedną listę"
  (example
   '((flatten '(1 (2) ((3)) ((((4)))))) (1 2 3 4)))

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
  "generuje ciąg liczb od *d* do *e* zwiększający się o *step*"
  (letrec ((I (lambda (s step e acc)
                (cond
                  ((>= s e) acc)
                  (else
                    (I (+ s step) step e (append acc (list s))))))))
    (I s step e '())))

(define (sum l)
  "sumuje wartości listy *l*"
  (apply + l))

; pt = (x . y), rect = (x y w h)
(define (point-in-rect? pt rect)
  "sprawdza czy punkt jest w prostokącie"
  (args
   '((pt . "punkt w postaci (x . y)")
     (rect . "prostokąt w postaci (x y w h)")))
  (let ((px (car pt))
        (py (cdr pt))
        (rx (list-ref rect 0))
        (ry (list-ref rect 1))
        (rw (list-ref rect 2))
        (rh (list-ref rect 3)))
    (and (>= px rx) (<= px (+ rx rw)) (>= py ry) (<= py (+ ry rh)))))

(define (split-every lst n)
  "dzieli listę *lst* co *n* elementów"
  (letrec ((f (lambda (in ret acc)
                (cond
                  ((null? in) (if (null? acc) ret (append ret (list acc))))
                  ((eqv? (length acc) n) (f in (append ret (list acc)) '()))
                  (else
                    (f (cdr in) ret (append acc (list (car in)))))))))
    (f lst '() '())))
