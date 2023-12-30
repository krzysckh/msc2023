(define (spawn-things)
  (let* ((xs (iota 0 50 800))
         (ys (map (→1 (exact->inexact (* (/ x 800) 600))) xs)))
    (for-each
     (→1 (create-source `((x . ,(list-ref xs x))
                          (y . ,(list-ref ys x)))))
     (iota 0 1 (length xs)))

    (create-mirror 0 300 400 600)))

(define current-thickness 1)
(define max-thickness 64)

(define (update-thicknesses)
  (for-each
   (→1 (set-source-e! x 'thickness current-thickness))
   (iota 0 1 (length *sources*))))

(define (kp-hook k d)
  (when *keypress-can-be-handled*
    (cond
     ((eqv? k #\s) (spawn-things))
     ((eqv? k #\+)
      (set! current-thickness (min (+ current-thickness 1) max-thickness))
      (update-thicknesses))
     ((eqv? k #\-)
      (set! current-thickness (max (- current-thickness 1) 1))
      (update-thicknesses)))))

(add-hook 'keypress kp-hook)

;; teścik dla gui/input-popup + wait + wszystko po drodze
(gui/input-popup
 "podaj imie"
 (lambda (imie)
   (let ((rysuj-id (add-hook
                    'frame
                    (→ (draw-text imie '(100 . 100) 50 pink)))))
     (wait 2 (→ (delete-hook 'frame rysuj-id))))))
