; TODO: HACK: to powinna byc funkcja pytajaca typu (can-handle-click?)
; i ogolnie user-hooki zalezne od tego i system-hooki niezalezne
(define *click-can-be-handled* #t)
(define *keypress-can-be-handled* #t)

(define *current-click-handler* #f)
(define *current-keypress-handler* #f)

;; ZMIANA POZYCJI DANEGO SOURCE_T
(define *source-size* 20) ;; TODO: ?????
(define repositioning-source #f)
(define repositioning-dx 0)
(define repositioning-dy 0)

(define (reposition-source-hook first left right)
  (when (or *click-can-be-handled* repositioning-source)
    (let ((mp (get-mouse-position)))
      (when (and first left)
        (for-each
          (lambda (n)
            (let* ((s (list-ref *sources* n))
                   (x (- (caar s) *source-size*))
                   (y (- (cdr (car s)) *source-size*)))
              (when (point-in-rect?
                     mp (list (+ x (/ *source-size* 2))
                              (+ y (/ *source-size* 2)) *source-size* *source-size*))
                (set! *click-can-be-handled* #f)
                (set! *current-click-handler* 'REPOSITION-SOURCE-HOOK)
                (set! repositioning-dx (- (car mp) x *source-size*))
                (set! repositioning-dy (- (cdr mp) y *source-size*))
                (set! repositioning-source n))))
          (iota 0 1 (length *sources*))))
      (when (and (not first) repositioning-source)
        (let ((pos (cons (- (car mp) repositioning-dx)
                         (- (cdr mp) repositioning-dy))))
          (set-source-e! repositioning-source 'pos pos))))))

(define (reposition-source-end-hook first left right)
  (when repositioning-source
    (set! *click-can-be-handled* #t)
    (set! repositioning-source #f)))

(add-system-hook 'click reposition-source-hook)
(add-system-hook 'unclick reposition-source-end-hook)

;; RYSOWANIE ZWIERCIADEŁ
(define mirror-last-x 0)
(define mirror-last-y 0)
(define drawing-new-mirror #f)

;; bardzo fajna nazwa, nie ma za co
;; ~ kpm

(define (start-drawing-mirror-hook first left right)
  (when (and (eqv? *current-mode* 'mirror-drawing-mode) (or *click-can-be-handled* drawing-new-mirror) (not right))
    (set! *click-can-be-handled* #f)
    (set! *current-click-handler* 'START-DRAWING-MIRROR-HOOK)
    (when (and first left)
      (set! drawing-new-mirror #t)
      (set! mirror-last-x (car (get-mouse-position)))
      (set! mirror-last-y (cdr (get-mouse-position))))
    (when (and (not first) drawing-new-mirror)
      (draw-line `(,mirror-last-x . ,mirror-last-y)
                 `(,(car (get-mouse-position)) . ,(cdr (get-mouse-position)))
                 2
                 (aq 'drawing-new-mirror *colorscheme*)))))

(define (end-drawing-mirror-hook first left right)
  (when drawing-new-mirror
    (set! *click-can-be-handled* #t)
    (set! drawing-new-mirror #f)
    (set! *current-mode* nil)
    (create-mirror
      mirror-last-x mirror-last-y
      (car (get-mouse-position)) (cdr (get-mouse-position)))))

(add-system-hook 'click start-drawing-mirror-hook)
(add-system-hook 'unclick end-drawing-mirror-hook)

;; używa *wait-alist* z scm/util.scm
(define (wait-handler time)
  "handler dla funkcji `(wait)`"
  (for-each
    (→1 ((cdr x)))
    (filter (→1 (<= (car x) time)) *wait-alist*))
  (set! *wait-alist* (filter (→1 (not (<= (car x) time))) *wait-alist*)))

(add-system-hook 'clock wait-handler)

(define (create-source-at-mouse-position)
  (create-source `((pos . ,(get-mouse-position)) (reactive . #t))))

;; DOMYŚLNE KEYBINDINGI
(define (keypress-default-hook c _)
  (when *keypress-can-be-handled*
    (let* ((mp (get-mouse-position)))
      (cond
        ((eqv? c #\A) (create-source-at-mouse-position))
        ((eqv? c #\e) (gui/input-popup "eval scheme" loads))
        ((eqv? c #\q) (exit 0))))))

(add-system-hook 'keypress keypress-default-hook)

(define (src->rect pos)
  (list (- (car pos) (/ *source-size* 2))
        (- (cdr pos) (/ *source-size* 2)) *source-size* *source-size*))

(define (gui/rect-fit-into-screen rect)
  (let ((x (list-ref rect 0))
        (y (list-ref rect 1))
        (w (list-ref rect 2))
        (h (list-ref rect 3)))
    (list
     (if (> (+ x w) *SCREEN-WIDTH*)
         (- x (- *SCREEN-WIDTH* (+ x w)))
         x)
     (if (> (+ y w) *SCREEN-HEIGHT*)
         (- y (- *SCREEN-HEIGHT* (+ y w)))
         y)
     w h)))

(define (gui/mp-slider+ok from to cb)
  (let* ((mp (get-mouse-position)))
    (letrec ((slider-dest
              (gui/slider
               (gui/rect-fit-into-screen (list (car mp) (cdr mp) 128 32))
               from to cb))
             (btn-dest
              (car (gui/btn (cons (car mp) (+ (cdr mp) 48))
                            "Ok"
                            (→ (slider-dest)
                               (btn-dest))))))
      0)))

(define source-settings
  `(("zmień kąt" . ,(→ (gui/mp-slider+ok
                       0 359 (→1 (set-source-e!
                                  source-settings-current-source-id
                                  'angle x)))))
    ("zmień ilość wiązek" . ,(→ (gui/mp-slider+ok
                                0 *source-size*
                                (→1 (set-source-e!
                                           source-settings-current-source-id
                                           'n-beams x)))))))
;; opts per source
(define source-settings-current-source-id nil) ;; TODO: hack lol
(add-hook
 'unclick
 (lambda (_ l r)
   (when (and *click-can-be-handled* r)
     (let ((mp (get-mouse-position)))
       (for-each
        (→1 (let ((pos (car (list-ref *sources* x))))
              (when (point-in-rect? mp (src->rect pos))
                (set! source-settings-current-source-id x)
                (gui/option-menu
                 (get-mouse-position)
                 source-settings))))
        (⍳ 0 1 (length *sources*)))))))

;; mouse-menu
(define mouse-menu
  `(("nowe źródło" . ,(→ (gui/new-source-form)))
    ("wyrażenie scheme" . ,(→ (gui/input-popup "eval scheme" loads)))
    ("narysuj zwierciadło" . ,(→ (when (eqv? *current-mode* nil)
                                   (set! *current-mode* 'mirror-drawing-mode))))))

(add-hook
 'unclick
 (lambda (first l r)
   (when (and *click-can-be-handled* r)
     (gui/option-menu (get-mouse-position) mouse-menu))))

;;;; tracelog
(define *tracelog-queue* '())

;; witam chcialem tylko powiedziec ze system tracelogow trzyma sie na dykcie i gownie
;; pozdrawiam serdecznie
;; ~ kpm
(add-hook
 'log
 (lambda (type s)
   (set! *tracelog-queue*
         (append *tracelog-queue* `(((s . ,s) (time . ,(time)) (type . ,type)))))))

(define (display-next-log)
  "ale fajna funkcja ciekawe jak dziala :333"
  (if (> (length *tracelog-queue*) 0)
    (let* ((tl (car *tracelog-queue*))
           (s (string-append
               "[" (number->string (aq 'time tl)) "] "
               (symbol->string (aq 'type tl)) ": "
               (aq 's tl)))
           (id (add-hook 'frame (→ (draw-text s '(0 . 0) 16 (aq 'font *colorscheme*) *default-spacing*)))))
      (set! *tracelog-queue* (cdr *tracelog-queue*))
      (wait 2 (→ (delete-hook 'frame id)
                 (display-next-log))))
    (letrec ((id (add-hook 'log (→2 (display-next-log)        ; korzystam tu z faktu,
                                    (delete-hook 'log id))))) ; że hooki wykonywane są kolejno
      nil)))                                                  ; (od najstarszych do najnowszych)

(display-next-log) ; lol

(add-hook
 'resize
 (→2 (set! *SCREEN-WIDTH* x)
     (set! *SCREEN-HEIGHT* y)))
