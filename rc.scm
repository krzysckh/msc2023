(define xs (iota 0 50 800))
(define ys (map (lambda (x) (exact->inexact (* (/ x 800) 600))) xs))

;(for-each
  ;(lambda (v)
    ;(create-source `((x . ,(list-ref xs v))
                     ;(y . ,(list-ref ys v)))))
  ;(iota 0 1 (length xs)))

;(create-mirror 0 300 400 600)

(define last-x 0)
(define last-y 0)
(define drawing-new-mirror #f)

(define (click-hook first left right)
  (when (and first left)
    (set! drawing-new-mirror #t)
    (set! last-x (car (get-mouse-position)))
    (set! last-y (cdr (get-mouse-position))))
  (when (and (not first) drawing-new-mirror)
    (draw-line `(,last-x . ,last-y)
               `(,(car (get-mouse-position)) . ,(cdr (get-mouse-position)))
               2
               '(0 0 255 255))))

(define (unclick-hook first left right)
  (when drawing-new-mirror
    (set! drawing-new-mirror #f)
    (create-mirror
      last-x last-y (car (get-mouse-position)) (cdr (get-mouse-position)))))

(add-hook 'click click-hook)
(add-hook 'unclick unclick-hook)
