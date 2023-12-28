(define (print s . l)
  (for-each (lambda (v) (display v) (display " ")) (append (list s) l))
  (newline))

(define (max2 a b) (if (> a b) a b))
(define (min2 a b) (if (< a b) a b))
(define (max . ns) (foldr max2 (car ns) ns))
(define (min . ns) (foldr min2 (car ns) ns))
(define (maxl lst) (apply max lst))
(define (minl lst) (apply min lst))
;; "functional programming"

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
