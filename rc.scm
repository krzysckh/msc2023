(define (_iota s step e acc)
  (cond
    ((>= s e) acc)
    (else
      (_iota (+ s step) step e (append acc (list s))))))

(define (iota s step e)
  (_iota s step e '()))

(define (print s)
  (display s)
  (newline))

(define xs (iota 0 50 800))
(define ys (map (lambda (x) (exact->inexact (* (/ x 800) 600))) xs))

(for-each
  (lambda (v) (create-source (list-ref xs v) (list-ref ys v) 20 90 #t))
  (iota 0 1 (length xs)))

(create-bounceable 0 300 400 600)

(define (kp-hook c)
  (print (list 'pressed c)))

(add-hook 'keypress kp-hook)
