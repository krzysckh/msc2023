(define (spawn-things)
  (let* ((xs (iota 0 50 800))
         (ys (map (lambda (x) (exact->inexact (* (/ x 800) 600))) xs)))
    (for-each
      (lambda (v)
        (create-source `((x . ,(list-ref xs v))
                         (y . ,(list-ref ys v)))))
      (iota 0 1 (length xs)))

    (create-mirror 0 300 400 600)))

(define current-thickness 1)
(define max-thickness 64)

(define (update-thicknesses)
  (for-each
   (lambda (n) (set-source-e! n 'thickness current-thickness))
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

(define *imie* nil)
(define rysuj-id nil)

(define (usun)
  (delete-hook 'frame rysuj-id))

(define (rysuj-imie)
  (draw-text *imie* '(100 . 100) 50 pink))

(define (podaj-imie-cb imie)
  (print "witaj, " imie)
  (set! *imie* imie)
  (set! rysuj-id (add-hook 'frame rysuj-imie))
  (wait 2 usun))

(gui/input-popup "podaj imie" podaj-imie-cb)
