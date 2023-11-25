(define (print s . l)
  (for-each (lambda (v) (display v) (display " ")) (append (list s) l))
  (newline))

(define (iota s step e)
  (letrec ((I (lambda (s step e acc)
                (cond
                  ((>= s e) acc)
                  (else
                    (I (+ s step) step e (append acc (list s))))))))
    (I s step e '())))

