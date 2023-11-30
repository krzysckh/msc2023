;(define xs (iota 0 50 800))
;(define ys (map (lambda (x) (exact->inexact (* (/ x 800) 600))) xs))

;(for-each
  ;(lambda (v)
    ;(create-source `((x . ,(list-ref xs v))
                     ;(y . ,(list-ref ys v)))))
  ;(iota 0 1 (length xs)))

;(create-mirror 0 300 400 600)

;(define (print-click _ __ ___)
  ;(draw-text "click" '(10 . 10) 20 '(0 0 0 255)))

;(add-user-hook 'click print-click)

;(define text-v "dupa")

;(define (try-gui)
  ;(set! text-v (real-gui-text-box 100 100 200 100 text-v 16 #t))
  ;#t)

(define (kp-hook k d)
  (when (eqv? k #\e)
    (gui/input-popup "eval scheme" loads)))

(add-hook 'keypress kp-hook)
