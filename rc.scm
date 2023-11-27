(define xs (iota 0 50 800))
(define ys (map (lambda (x) (exact->inexact (* (/ x 800) 600))) xs))

(for-each
  (lambda (v)
    (create-source `((x . ,(list-ref xs v))
                     (y . ,(list-ref ys v)))))
  (iota 0 1 (length xs)))

(create-mirror 0 300 400 600)
