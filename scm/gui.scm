; TODO: sprawdź czy przypadkiem używanie wbudowanej funkcji w raylib nie będzie
; szybsze

(define *current-mode* nil)
(define gui/draw-text draw-text)

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

(define (gui/window-box-get-empty-space rect)
  (list (car rect) (+ (cadr rect) gui/window-top-bar-size)
        (caddr rect) (- (cadddr rect) gui/window-top-bar-size)))

(define (gui/window-box rect title)
  "rysuje bounding-box okienka wraz z tytułem, zwraca miejsce, które pozostało na elementy"
  (args
   '((rect . "prostokąt `(x y w h)`")
     (title . "tytuł")))

  (gui/rect rect (aq 'frame *colorscheme*)) ; bounding box
  (gui/rect `(,(car rect) ,(cadr rect) ,(caddr rect) ,gui/window-top-bar-size)
            (aq 'frame *colorscheme*))
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) (aq 'font *colorscheme*))
  (gui/window-box-get-empty-space rect))

(define (gui/window-box-retained rect title)
  "rysuje window-box, tylko, że dodaje hooki dla 'frame. zwraca `(destruktor to-co-gui/window-box)`"
  (let ((frame-id (add-hook 'frame (→ (gui/window-box rect title)))))
    (list
     (→ (delete-hook 'frame frame-id))
     (gui/window-box-get-empty-space rect))))

(define (gui/input-box rect text)
  (error "not implemented"))

(define (gui/label rect text)
  (draw-text title `(,(car rect) . ,(+ 1 (cadr rect)))
             (- gui/window-top-bar-size 2) (aq 'font *colorscheme*)))

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
                                              (* n text-height) 2)) 18 (aq 'font *colorscheme*)))
      (iota 0 1 (length text)))))

;; TODO: popup powinien zostać rozłożony na pomniejsze funkcje
;; gui/input-popup
(define gui/input-popup:ident 'GUI-input-popup)
(define (gui/input-popup title callback)
  (when *click-can-be-handled*
    (stop-simulation)
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
                     (start-simulation)
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
;; TODO: uzyj gui/button zamiast ad-hoc sztynksów
(define *gui/option-menu-text-size* 16)
(define gui/option-menu:ident 'GUI-option-menu)
(define (gui/option-menu pos opts)
  (args
   '((pos . "pozycja lewego-górnego punktu opcji w formie `(x . y)`")
     (opts . "opcje w formie ((tekst . funkcja) (tekst . funkcja) ...)")))

  (when (and *click-can-be-handled* (eqv? *current-mode* nil))
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
                   (fill-rect (list-ref rects n) (aq 'background *colorscheme*))
                   (gui/rect (list-ref rects n) (aq 'frame *colorscheme*))
                   (draw-text
                    (car (list-ref opts n))
                    (cons x (+ y (* n single-h)))
                    *gui/option-menu-text-size* (aq 'font *colorscheme*)))
                 (iota 0 1 (length opts))))))
           (click-id
            (add-hook
             'unclick
             (lambda (first l r)
               (when l
                 (let ((mp (get-mouse-position)))
                   (set! *click-can-be-handled* #t)
                   (set! *current-click-handler* nil)
                   (delete-hook 'unclick click-id)
                   (delete-hook 'frame frame-id)
                   (when (point-in-rect? mp real-rect)
                     (for-each
                      (→1 (when (point-in-rect? mp (list-ref rects x))
                            ((cdr (list-ref opts x)))))
                      (iota 0 1 (length opts)))))))))))))

;;; gui/button
;; to troche posrana i skomplikowana sprawa, ale że nie mam innego pomysłu to cusz
;; ~ kpm

;; TODO: zmienic to na cos co oblicza gdzie jest srodek przycisku
(define gui/button:padding 2)
(define gui/button:text-size 16)
(define gui/button:text-spacing 1)
(define *gui/button-force-can-be-handled* #f)

(define (gui/button rect text cb)
  "tworzy przycisk w `rect` z tekstem text. po przyciśnięciu wykonuje `cb`.
zwraca **destruktor** - funkcję usuwającą go"
  (let ((frame-id
         (add-hook
          'frame
          (→ (gui/rect rect (aq 'frame *colorscheme*))
             (draw-text
              text
              (cons
               (+ (list-ref rect 0) gui/button:padding)
               (+ (list-ref rect 1) gui/button:padding))
              16 (aq 'font *colorscheme*) 1))))
        (click-id
         (add-hook
          'unclick
          (→3
           (when (and (or *click-can-be-handled* *gui/button-force-can-be-handled*)
                      (point-in-rect? (get-mouse-position) rect))
             (cb))))))
    (→ (delete-hook 'frame frame-id)
       (delete-hook 'unclick click-id))))

(define (gui/btn pos text cb)
  "wykonuje gui/button, tylko sam liczy jak szeroki i wysoki ma być przycisk się zmieścił. zwraca (destruktor szerokosc wysokosc)"
  (args '((pos . "pozycja w formacie (x . y)")
          (text . "tekst w przycisku")
          (cb . "callback")))
  (let* ((measure (measure-text text gui/button:text-size gui/button:text-spacing))
         (w (+ (* 2 gui/button:padding) (car measure)))
         (h (+ (* 2 gui/button:padding) (cdr measure))))
    (list (gui/button `(,(car pos) ,(cdr pos) ,w ,h) text cb)
          w h)))

;;; gui/slider

(define *gui/slider-force-can-be-handled* #t)
(define gui/slider:ident 'GUI-slider)

(define (gui/slider rect from to cb)
  "tworzy slider. wywołuje `cb` z wynikiem za każdym `'unclick` eventem. zwraca destruktor."
  (args
   '((rect . "w formacie `(x y w h)`")
     (from . "minimum")
     (to   . "maksimum")
     (cb   . "callback")))

  (let* ((sl-h (/ (list-ref rect 3) 2))
         (sl-w (list-ref rect 2))
         (inner-rect (list (list-ref rect 0)
                           (+ (list-ref rect 1)
                              (/ sl-h 2))
                           sl-w
                           sl-h))
         (slider-rect nil)
         (slider-rect-w 10)
         (maxx (- (+ (car rect) sl-w (/ slider-rect-w 2)) slider-rect-w))
         (minx (- (car rect) (/ slider-rect-w 2)))
         (current-x minx)
         (holding #f)
         (update-slider-rect
          (lambda () (set!
                 slider-rect
                 `(,current-x ,(list-ref rect 1) ,slider-rect-w ,(list-ref rect 3)))))
         (_ (update-slider-rect))
         (frame-id
          (add-hook
           'frame
           (→ (gui/rect inner-rect (aq 'frame *colorscheme*))
              (update-slider-rect)
              (fill-rect slider-rect (aq 'frame *colorscheme*))

              (let ((mp (get-mouse-position))) ;; HACK
                (when *click-can-be-handled*
                  (if (point-in-rect? mp rect)
                      (begin
                        (set! *current-click-handler* gui/slider:ident)
                        (set! *click-can-be-handled* #f))
                      (set! *click-can-be-handled* #t)))))))

         (click-id
          (add-hook
           'click
           (lambda (first l r)
             (let ((mp (get-mouse-position)))
               (when (or (and (or (point-in-rect? mp inner-rect) (point-in-rect? mp slider-rect))
                              (or (or *click-can-be-handled* *gui/slider-force-can-be-handled*)
                                  (eqv? *current-click-handler* gui/slider:ident))
                              l
                              first)
                         holding)
                 (set! holding #t)
                 (set! *click-can-be-handled* #f)
                 (set! current-x (max2 minx (min2 maxx (car mp))))
                 (let* ((v (- current-x minx))
                        (∆range (- to from))
                        (∆ (/ v sl-w))
                        (r (* ∆range ∆)))
                   (cb (+ r from))))))))
         (unclick-id
          (add-hook
           'unclick
           (→3 (when (or (eqv? *current-click-handler* gui/slider:ident) *gui/slider-force-can-be-handled*)
                 (set! holding #f)
                 (set! *current-click-handler* nil)
                 (when (not *gui/slider-force-can-be-handled*)
                   (set! *click-can-be-handled* #t)))))))

    (→ (delete-hook 'frame frame-id)
       (delete-hook 'click click-id)
       (delete-hook 'unclick unclick-id))))

;;; gui/checkbox

(define *gui/checkbox-force-can-be-handled* #t)
(define (gui/checkbox rect cb . state)
  "tworzy checkbox. zwraca destruktor."
  (args
   '((rect . "prostokąt na checkbox")
     (cb . "callback wykonywany po kliknięciu, jako argument przekazuje aktualną wartość (#t | #f)")
     (state . "(opcjonalnie) początkowa wartość (#t | #f)")))

  (define checked (if (null? state) #f (car state)))
  (let* ((padding (/ (list-ref rect 3) 4))
         (checked-rect (list (+ padding (list-ref rect 0))
                             (+ padding (list-ref rect 1))
                             (- (- (list-ref rect 2) 1) (* 2 padding))
                             (- (- (list-ref rect 3) 1) (* 2 padding))))
         (frame-id
          (add-hook
           'frame
           (→ (gui/rect rect (aq 'frame *colorscheme*))
              (when checked
                (fill-rect checked-rect (aq 'frame *colorscheme*))))))

         (unclick-id
          (add-hook
           'unclick
           (lambda (_ l r)
             (when (or *click-can-be-handled* *gui/checkbox-force-can-be-handled*)
               (when (point-in-rect? (get-mouse-position) rect)
                 (set! checked (not checked))
                 (cb checked)))))))

    (→ (delete-hook 'frame frame-id)
       (delete-hook 'unclick unclick-id))))

(define (gui/draw-text-persist . args)
  "zostawia narysowany tekst. zwraca destruktor. ***argumenty jak do `(draw-text)`***"
  (let ((id (add-hook 'frame (→ (apply draw-text args)))))
    (→ (delete-hook 'frame id))))

(define gui/new-source-form:padding 40)
(define (gui/new-source-form)
  "form pytający użytkownika o dane nowego źródła"

  (stop-simulation)
  (set! *click-can-be-handled* #f)
  (set! *keypress-can-be-handled* #f)

  (set! *gui/slider-force-can-be-handled* #t)
  (set! *gui/button-force-can-be-handled* #t)
  (set! *gui/checkbox-force-can-be-handled* #t)

  (define n-beams 1)
  (define mouse-reactive #t)
  (define angle 0)

  (let* ((window-box-rect
          (list
           gui/new-source-form:padding
           gui/new-source-form:padding
           (- *SCREEN-WIDTH* (* 2 gui/new-source-form:padding))
           (- *SCREEN-HEIGHT* (* 2 gui/new-source-form:padding))))
         (window-box-data (gui/window-box-retained window-box-rect "nowe źródło"))
         (d-window-box (car window-box-data))
         (rect (cadr window-box-data))
         (d-n-beam-slider (gui/slider
                           (list (+ 10 (car rect))
                                 (+ 10 (cadr rect))
                                 128
                                 32)
                           1 20 (→1 (set! n-beams (round x)))))
         (d-n-beam-label
          (let ((id (add-hook
                    'frame
                    (→
                     (gui/draw-text
                      (string-append "ilość wiązek: " (number->string n-beams))
                      (cons (+ 10 (car rect) 128 10)
                            (+ 10 (cadr rect) (/ (cdr (measure-text "A" 16)) 2)))
                      16 (aq 'font *colorscheme*))))))
            (→ (delete-hook 'frame id))))
         (_1-line-height (+ 10 (cadr rect) 32))

         (d-mouse-r-checkbox (gui/checkbox (list (+ (car rect) 10)
                                                 (+ 10 _1-line-height)
                                                 20 20)
                                           (→1 (set! mouse-reactive x))
                                           mouse-reactive))
         (d-mouse-r-label (gui/draw-text-persist
                           "czy wiązka wskazuje na myszkę?"
                           (cons (+ (car rect) 10 16 10)
                                 (+ _1-line-height (/ (cdr (measure-text "A" 16)) 2)))
                           16 (aq 'font *colorscheme*)))

         (_2-line-height (+ _1-line-height 32))

         (d-angle-slider (gui/slider
                           (list (+ 10 (car rect))
                                 (+ 10 _2-line-height)
                                 128
                                 32)
                           0 360 (→1 (set! angle (round x)))))
         (d-angle-label
          (let ((id (add-hook
                    'frame
                    (→
                     (gui/draw-text
                      (string-append "kąt: " (number->string angle))
                      (cons (+ 10 (car rect) 128 10)
                            (+ 10 _2-line-height (/ (cdr (measure-text "A" 16)) 2)))
                      16 (aq 'font *colorscheme*))))))
            (→ (delete-hook 'frame id))))

         ;; końcowy przycisk "ok"
         (d-OK-btn (car (gui/btn (cons (+ (car rect) 10)
                                       (- *SCREEN-HEIGHT* 80))
                                 "Ok"
                                 (→
                                  (print "reactive: " mouse-reactive)
                                  (create-source
                                   `((n-beams . ,n-beams)
                                     (reactive . ,mouse-reactive)
                                     (angle . ,angle)))

                                  ;; cleanup gui
                                  (d-window-box)
                                  (d-n-beam-slider)
                                  (d-n-beam-label)

                                  (d-mouse-r-checkbox)
                                  (d-mouse-r-label)

                                  (d-angle-slider)
                                  (d-angle-label)

                                  (d-OK-btn)

                                  ;; cleanup vars
                                  (set! *click-can-be-handled* #t)
                                  (set! *keypress-can-be-handled* #t)
                                  (set! *gui/slider-force-can-be-handled* #f)
                                  (set! *gui/button-force-can-be-handled* #f)
                                  (set! *gui/checkbox-force-can-be-handled* #f)

                                  (start-simulation)
                                  )))))
    nil))
