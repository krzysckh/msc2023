; TODO: sprawdź czy przypadkiem używanie wbudowanej funkcji w raylib nie będzie
; szybsze
(define (gui/rect rect)
  (let ((x (list-ref rect 0))
        (y (list-ref rect 1))
        (w (list-ref rect 2))
        (h (list-ref rect 3)))
    (draw-line `(,x . ,y)       `(,(+ x w) . ,y)       1 black)
    (draw-line `(,x . ,(+ y h)) `(,(+ x w) . ,(+ y h)) 1 black)
    (draw-line `(,x . ,y)       `(,x       . ,(+ y h)) 1 black)
    (draw-line `(,(+ x w) . ,y) `(,(+ x w) . ,(+ y h)) 1 black)))

(define gui/window-top-bar-size 16)

(define (gui/window-box rect title)
  (gui/rect rect) ; bounding box
  (gui/rect `(,(car rect) ,(cadr rect) ,(caddr rect) ,gui/window-top-bar-size))
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) black))

(define (gui/input-box rect text)
  (error "not implemented"))

(define (gui/label rect text)
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) black))


;; gui/input-popup
(define gui/input-popup-state "")
(define gui/input-popup-key-handler-id #f)
(define gui/input-popup-frame-handler-id #f)
(define gui/input-popup-callback #f)

(define (gui/input-popup-key-handler c k)
  (if (and (< k 128) (> k 31))
    (set! gui/input-popup-state (string-append gui/input-popup-state (string c)))
    (cond
      ((eqv? k 259)
       (set! gui/input-popup-state
         (substring
           gui/input-popup-state 0
           (- (string-length gui/input-popup-state) 1))))
      ((eqv? k 257)
       (gui/end-input-popup)))))

(define (gui/input-popup-frame-handler)
  (draw-text gui/input-popup-state '(100 . 100) 16 black))

(define (gui/input-popup text callback)
  (define ident 'GUI-input-popup)

  (if *click-can-be-handled*
    (begin
      (set! gui/input-popup-callback callback)
      (set! *click-can-be-handled* #f)
      (set! *keypress-can-be-handled* #f)
      (set! *current-keypress-handler* ident)
      (set! *current-click-handler*    ident)

      (set! gui/input-popup-state "")
      (set! gui/input-popup-frame-handler-id
        (add-hook 'frame gui/input-popup-frame-handler))
      (set! gui/input-popup-key-handler-id
        (add-hook 'keypress gui/input-popup-key-handler)))
    #f))

(define (gui/end-input-popup)
  (set! *click-can-be-handled* #t)
  (set! *keypress-can-be-handled* #t)
  (set! *current-keypress-handler* #f)
  (set! *current-click-handler* #f)
  (delete-hook 'frame gui/input-popup-frame-handler-id)
  (delete-hook 'keypress gui/input-popup-key-handler-id)
  (print "calling back with" gui/input-popup-state)
  (gui/input-popup-callback gui/input-popup-state))

