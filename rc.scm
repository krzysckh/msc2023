(define (spawn-things)
  (let* ((xs (iota 0 50 800))
         (ys (map (lambda (x) (exact->inexact (* (/ x 800) 600))) xs)))
    (for-each
      (lambda (v)
        (create-source `((x . ,(list-ref xs v))
                         (y . ,(list-ref ys v)))))
      (iota 0 1 (length xs)))

    (create-mirror 0 300 400 600)))

(define (kp-hook k d)
  (when *keypress-can-be-handled*
    (cond
      ((eqv? k #\e) (gui/input-popup "eval scheme" loads))
      ((eqv? k #\q) (exit 0))
      ((eqv? k #\s) (spawn-things)))))

(add-hook 'keypress kp-hook)
