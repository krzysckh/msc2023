(define (keypress-default-hook c)
  (let* ((mp (get-mouse-position))
         (mx (car mp))
         (my (cdr mp)))
    (cond
      ((eqv? c #\A) (create-source mx my 20 90 #t)))))

(add-hook 'keypress keypress-default-hook)
