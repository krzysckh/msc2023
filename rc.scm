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

(define (show-fps pos)
  (let* ((cur-fps 0)
         (fps 0)
         (frame-id
          (add-hook 'frame (→ (draw-text
                               (string-append "fps: " (number->string cur-fps))
                               pos 21 (aq 'font *colorscheme*))
                              (++ fps))))
         (clock-id
          (add-hook 'clock (→ (set! cur-fps fps)
                              (set! fps 0)))))
    (→ (delete-hook 'frame frame-id)
       (delete-hook 'clock clock-id))))

(define hookable '(keypress click unclick resize clock log new update frame))
(define (show-hook-status)
  (let ((id (add-hook
             'frame
             (→ (for-each
                 (→1 (let* ((sym (list-ref hookable x))
                            (n (length (get-all-hooks sym))))
                       (draw-text (string-append (symbol->string sym) " " (number->string n))
                                  (cons 16 (- *SCREEN-HEIGHT* 32 (+ 10 (* x 20))))
                                  16
                                  white)))
                 (⍳ 0 1 (length hookable)))))))
    (→ (delete-hook 'frame id))))

(define hook-status-dest nil)
(define window-opts-dest nil)
(define fps-dest nil)
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
      (update-thicknesses))
     ((eqv? k #\o)
      (if (null? window-opts-dest)
          (set! window-opts-dest (gui/show-window-opts))
          (begin
            (window-opts-dest)
            (set! window-opts-dest nil))))
     ((eqv? k #\?)
      (if (null? hook-status-dest)
          (set! hook-status-dest (show-hook-status))
          (begin
            (hook-status-dest)
            (set! hook-status-dest nil))))
     ((eqv? k #\f)
      (if (null? fps-dest)
          (set! fps-dest (show-fps '(16 . 16)))
          (begin
            (fps-dest)
            (set! fps-dest nil)))))))

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

;; (for-each rand-mirror (⍳ 0 1 128))
(rand-mirror)
