(define gui/font-color '(#xd3 #xc6 #xaa #xff))
(define gui/frame-color '(#xdb #xbc #x7f #xff))

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
  "rysuje bounding-box okienka wraz z tytułem, zwraca miejsce, które pozostało na elementy"
  (args
   '((rect . "prostokąt `(x y w h)`")
     (title . "tytuł")))

  (gui/rect rect gui/frame-color) ; bounding box
  (gui/rect `(,(car rect) ,(cadr rect) ,(caddr rect) ,gui/window-top-bar-size)
            gui/frame-color)
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) gui/font-color)
  (list (car rect) (+ (cadr rect) gui/window-top-bar-size)
        (caddr rect) (- (cadddr rect) gui/window-top-bar-size)))

(define (gui/input-box rect text)
  (error "not implemented"))

(define (gui/label rect text)
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) gui/font-color))

(define (gui/get-max-text-length-for-width w sz)
  (letrec ((f (lambda (s)
                (cond
                  ((>= (car (measure-text s sz)) w) (- (string-length s) 1))
                  (else
                    (f (string-append s "a")))))))
    (f "a")))

(define (gui/multiline-text rect txt)
  (let* ((w (list-ref rect 2))
         (text-height (cdr (measure-text "a" 18)))
         (max-len (gui/get-max-text-length-for-width w 18))
         (text (map list->string (split-every (string->list txt) max-len))))
    (for-each
      (lambda (n)
        (draw-text (list-ref text n)
                   `(,(list-ref rect 0) . ,(+ (list-ref rect 1)
                                              (* n text-height) 2)) 18 gui/font-color))
      (iota 0 1 (length text)))))

;; TODO: popup powinien zostać rozłożony na pomniejsze funkcje
;; gui/input-popup
(define gui/input-popup:ident 'GUI-input-popup)
(define (gui/input-popup title callback)
  (when *click-can-be-handled*
    (define state "")
    (set! *click-can-be-handled* #f)
    (set! *keypress-can-be-handled* #f)
    (set! *current-keypress-handler* gui/input-popup:ident)
    (set! *current-click-handler*    gui/input-popup:ident)

    (let* ((frame-handler-id
            (add-hook
             'frame
             (→ (gui/window-box '(100 100 600 400) title)
                (gui/multiline-text '(200 200 400 200) state))))
           (key-handler-id
            (add-hook
             'keypress
             (lambda (c k)
               (if (and (< k 128) (> k 31))
                   (set! state
                         (string-append state (string c)))
                   (cond
                    ((eqv? k 259)
                     (set! state
                           (substring
                            state 0
                            (- (string-length state) 1))))
                    ((eqv? k 257) ;; RET (end popup)
                     (set! *click-can-be-handled* #t)
                     (set! *keypress-can-be-handled* #t)
                     (set! *current-keypress-handler* #f)
                     (set! *current-click-handler* #f)
                     (delete-hook 'frame frame-handler-id)
                     (delete-hook 'keypress key-handler-id)
                     (callback state)))))))))))

(define (gui/message title text timeout . rect)
  "wyświetla wiadomość"
  (args
   '((title . "tytuł przekazany do gui/window-box")
     (text . "tekst wiadomości")
     (timeout . "czas po którym wiadomość znika")
     (rect . "rect dla wiadomości (nieobowiązkowy)")))

  (let* ((r (if (null? rect) '(10 10 300 100) (car rect)))
         (id (add-hook 'frame
                       (→ (let ((R (gui/window-box r title)))
                            (gui/multiline-text R text))))))
    (wait timeout (→ (delete-hook 'frame id)))))

(define (gui/msg text)
  "wyświetla gui/message"
  (gui/message "" text 5))

;; option-menu
(define *gui/option-menu-text-size* 16)
(define gui/option-menu:ident 'GUI-option-menu)
(define (gui/option-menu pos opts)
  (args
   '((pos . "pozycja lewego-górnego punktu opcji w formie `(x . y)`")
     (opts . "opcje w formie ((tekst . funkcja) (tekst . funkcja) ...)")))

  (when *click-can-be-handled*
    (set! *click-can-be-handled* #f)
    (set! *current-click-handler* gui/option-menu:ident)
    (let* ((measures (map (→1 (measure-text x *gui/option-menu-text-size*)) (map car opts)))
           (w (maxl (map car measures)))
           (h (+ (sum (map cdr measures)) (* 4 (length opts))))
           (x (if (> (+ w (car pos)) *SCREEN-WIDTH*) (- *SCREEN-WIDTH* w) (car pos)))
           (y (if (> (+ h (cdr pos)) *SCREEN-HEIGHT*) (- *SCREEN-HEIGHT* h) (cdr pos)))
           (single-h (avg (map cdr measures)))
           (real-rect (list x y w h))
           (rects (map (lambda (n) (list x (+ y (* n single-h)) w single-h))
                       (iota 0 1 (length opts))))
           (frame-id
            (add-hook
             'frame
             (→ (for-each
                 (lambda (n)
                   (gui/rect (list-ref rects n) gui/frame-color)
                   (draw-text
                    (car (list-ref opts n))
                    (cons x (+ y (* n single-h)))
                    *gui/option-menu-text-size* gui/font-color))
                 (iota 0 1 (length opts))))))
           (click-id
            (add-hook
             'unclick
             (lambda (first l r)
               (when l
                 (let ((mp (get-mouse-position)))
                   (when (point-in-rect? mp real-rect)
                     (set! *click-can-be-handled* #t)
                     (set! *current-click-handler* nil)
                     (delete-hook 'unclick click-id)
                     (delete-hook 'frame frame-id)
                     (for-each
                      (→1 (when (point-in-rect? mp (list-ref rects x))
                            ((cdr (list-ref opts x)))))
                      (iota 0 1 (length opts)))))))))))))
