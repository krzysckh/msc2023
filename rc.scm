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
  (tracelog 'info "updating thickness to " current-thickness)
  (for-each
   (→1 (set-source-e! x 'thickness current-thickness))
   (iota 0 1 (length *sources*))))

(define (kp-hook k d)
  (when *keypress-can-be-handled*
    (cond
     ((eqv? k #\s) (spawn-things))
     ((eqv? k #\m) (rand-mirror))
     ((eqv? k #\+)
      (set! current-thickness (min (+ current-thickness 1) max-thickness))
      (update-thicknesses))
     ((eqv? k #\-)
      (set! current-thickness (max (- current-thickness 1) 1))
      (update-thicknesses)))))

(add-hook 'keypress kp-hook)

;; TODO: gui/compose

;; (define (set-bgcolor col)
;;   (let ((wc (get-winconf)))
;;     (apply set-winconf (append (list col) (cdr wc)))))

;; (set-bgcolor black)

;; (define (toggle-flag flag)
;;   (set-window-flag flag (not (get-window-flag flag))))


(set! *seed* (time))
(define % modulo)
(define (rand-mirror)
  (add-mirror
   (cons (% (random-next) *SCREEN-WIDTH*)
         (% (random-next) *SCREEN-HEIGHT*))
   (cons (% (random-next) *SCREEN-WIDTH*)
         (% (random-next) *SCREEN-HEIGHT*))))

;; (create-source '((pos . (20 . 200))))


;; (create-prism '(200 . 200) 100 1.31)

;; (rand-mirror)

;; (add-hook
;;  'frame
;;  (→ (let* ((vs (cdr (get-bounceable 1)))
;;            (_ (print vs))
;;            (pos (car vs))
;;            (new-pos (cons (car pos) (+ 1 (cdr pos)))))
;;       (set-prism! 1 new-pos 100 1.31))))

;; (wait 2 (→ (delete-bounceable 0)))

