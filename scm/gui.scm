; TODO: sprawdź czy przypadkiem używanie wbudowanej funkcji w raylib nie będzie
; szybsze
(define (gui/rect rect c)
  (let ((x (list-ref rect 0))
        (y (list-ref rect 1))
        (w (list-ref rect 2))
        (h (list-ref rect 3)))
    (draw-line `(,x . ,y)       `(,(+ x w) . ,y)       1 c)
    (draw-line `(,x . ,(+ y h)) `(,(+ x w) . ,(+ y h)) 1 c)
    (draw-line `(,x . ,y)       `(,x       . ,(+ y h)) 1 c)
    (draw-line `(,(+ x w) . ,y) `(,(+ x w) . ,(+ y h)) 1 c)))

(define gui/window-top-bar-size 16)

(define (gui/window-box rect title)
  (gui/rect rect black) ; bounding box
  (gui/rect `(,(car rect) ,(cadr rect) ,(caddr rect) ,gui/window-top-bar-size)
            black)
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) black))

(define (gui/input-box rect text)
  (error "not implemented"))

(define (gui/label rect text)
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) black))

(define (gui/get-max-text-length-for-width w sz)
  (letrec ((f (lambda (s)
                (cond
                  ((>= (car (measure-text s sz)) w) (- (string-length s) 1))
                  (else
                    (f (string-append s "a")))))))
    (f "a")))

(define (gui/multiline-text rect text)
  (let* ((w (list-ref rect 2))
         (text-height (cdr (measure-text "a" 18)))
         (max-len (gui/get-max-text-length-for-width w 18))
         (text (map list->string (split-every
                                   (string->list gui/input-popup-state)
                                   max-len))))
    (for-each
      (lambda (n)
        (draw-text (list-ref text n)
                   `(,(list-ref rect 0) . ,(+ (list-ref rect 1)
                                              (* n text-height) 2)) 18 black))
      (iota 0 1 (length text)))))

;; TODO: popup powinien zostać rozłożony na pomniejsze funkcje
;; gui/input-popup
(define gui/input-popup-state "")
(define gui/input-popup-key-handler-id nil)
(define gui/input-popup-frame-handler-id nil)
(define gui/input-popup-callback nil)
(define gui/input-popup-title nil)

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
  (gui/window-box '(100 100 600 400) gui/input-popup-title)
  (gui/multiline-text '(200 200 400 200) gui/input-popup-state))

(define (gui/input-popup title callback)
  (define ident 'GUI-input-popup)

  (if *click-can-be-handled*
    (begin
      (set! gui/input-popup-title title)
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
  (gui/input-popup-callback gui/input-popup-state))

