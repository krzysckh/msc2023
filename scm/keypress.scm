(define (keypress-default-hook c)
  (let* ((mp (get-mouse-position))
         (mx (car mp))
         (my (cdr mp)))
    (cond
      ((eqv? c #\A) (create-source `((x . ,mx) (y . ,my)))))))

(add-hook 'keypress keypress-default-hook)
