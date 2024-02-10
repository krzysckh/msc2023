; TODO: HACK: to powinna byc funkcja pytajaca typu (can-handle-click?)
; i ogolnie user-hooki zalezne od tego i system-hooki niezalezne
(define *current-mode* nil)

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
                (set-cursor MOUSE-CURSOR-RESIZE-ALL)
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
    (set-cursor MOUSE-CURSOR-DEFAULT)
    (set! *click-can-be-handled* #t)
    (set! repositioning-source #f)))

(add-system-hook 'click reposition-source-hook)
(add-system-hook 'unclick reposition-source-end-hook)

;; RYSOWANIE ZWIERCIADEŁ
(define mirror-last-x 0)
(define mirror-last-y 0)
(define drawing-new-mirror #f)

(define (start-drawing-mirror-hook first left right)
  (when (and (eqv? *current-mode* 'mirror-drawing) (or *click-can-be-handled* drawing-new-mirror) (not right))
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
    (set-cursor MOUSE-CURSOR-DEFAULT)
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

;; opts per source
(add-hook
 'unclick
 (lambda (_ l r)
   (when (and *click-can-be-handled* r)
     (let ((mp (get-mouse-position)))
       (for-each
        (→1 (let ((pos (car (list-ref *sources* x)))
                  (cur (list-ref *sources* x)))
              (when (point-in-rect? mp (src->rect pos))
                (let ((source-settings
                       `(("zmień kąt" . ,(→ (gui/mp-slider+ok
                                             0 359
                                             (lambda (v) (set-source-e! x 'angle v)))))
                         ("'mouse-reactive" . ,(→ (set-source-e! x 'mouse-reactive (not (list-ref cur 3)))))
                         ("zmień ilość wiązek" . ,(→ (gui/mp-slider+ok
                                                      0 *source-size*
                                                      (lambda (v) (set-source-e! x 'n-beams v))))))))
                  (gui/option-menu (get-mouse-position) source-settings)))))
        (⍳ 0 1 (length *sources*)))))))

;; mouse-menu
(define mouse-menu
  `(("nowe źródło" . ,(→ (set! gui/new-source-form:pos (get-mouse-position))
                         (gui/new-source-form)))
    ("narysuj zwierciadło" . ,(→ (when (eqv? *current-mode* nil)
                                   (set-cursor MOUSE-CURSOR-CROSSHAIR)
                                   (tracelog 'info "narysuj nowe zwierciadło...")
                                   (set! *current-mode* 'mirror-drawing))))
    ("wyrażenie scheme" . ,(→ (gui/input-popup "eval" loads)))
    ("wyczyść *tracelog-queue*" . ,(→ (set! *tracelog-queue* nil)))))

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

;;; selection-mode
(define sel-mode:start-position nil)
(define sel-mode:mirror-rects nil)
(define sel-mode:selected-mirror-ids nil)
(define sel-mode:last-time 0)

(define (rect-collision? r1 r2)
  (not (eqv? (sum (rect-collision r1 r2)) 0.0)))

(add-hook
 'click
 (lambda (first l r)
   (when (and l (> (time) sel-mode:last-time))
     (when (and (not first) *click-can-be-handled*)
       (set! first #t))
     (when (or (and *click-can-be-handled* (eqv? *current-mode* nil)) (eqv? *current-mode* 'selection))
       (if first
           (begin
             (set! *click-can-be-handled* #f)
             (set! sel-mode:start-position (get-mouse-position))
             (set! *current-mode* 'selection)
             (set! sel-mode:mirror-rects (map (→1 (apply pts->rect x)) (map cdr *mirrors*))))
           (begin
             (let* ((mp (get-mouse-position))
                    (rect (list
                           (car sel-mode:start-position)
                           (cdr sel-mode:start-position)
                           (- (car mp) (car sel-mode:start-position))
                           (- (cdr mp) (cdr sel-mode:start-position))))
                    (selected-mirror-map
                     (map (→1 (rect-collision? rect x)) sel-mode:mirror-rects)))
               (for-each
                (→1 (gui/rect
                     (list-ref sel-mode:mirror-rects x)
                     (if (list-ref selected-mirror-map x)
                         (aq 'green *colorscheme*)
                         (aq 'red *colorscheme*))))
                (⍳ 0 1 (length sel-mode:mirror-rects)))
               (gui/rect rect (aq 'selection *colorscheme*)))))))))

(add-hook
 'unclick
 (→3 (when (and y (eqv? *current-mode* 'selection))
       (set! *current-mode* 'selected)
       (set!
        sel-mode:selected-mirror-ids
        (map car
             (filter
              (→1 (let ((mp (get-mouse-position)))
                    (rect-collision?
                     (pts->rect (cadr x) (caddr x))
                     (list
                      (car sel-mode:start-position)
                      (cdr sel-mode:start-position)
                      (- (car mp) (car sel-mode:start-position))
                      (- (cdr mp) (cdr sel-mode:start-position))))))
              *mirrors*)))

       (start-selected-mode))))

(define (start-selected-mode)
  (stop-simulation)
  (if (> (length sel-mode:selected-mirror-ids) 0)
      (let* ((mp (get-mouse-position))
             (menu-open #f)
             (real-sel-mirrors (map (→1 (assq x *mirrors*)) sel-mode:selected-mirror-ids))
             (sel-mirrors (map cdr real-sel-mirrors))
             (p1s (map car sel-mirrors))
             (p2s (map cadr sel-mirrors))
             (minx (minl (append (map car p1s) (map car p2s))))
             (miny (minl (append (map cdr p1s) (map cdr p2s))))
             (maxx (maxl (append (map car p1s) (map car p2s))))
             (maxy (maxl (append (map cdr p1s) (map cdr p2s))))
             (∆x 0)
             (∆y 0)
             (∆mouse nil)
             (bounding-rect nil)
             (update-bounding-rect
              (→ (set! bounding-rect (list (+ ∆x minx)
                                           (+ ∆y miny)
                                           (- maxx minx)
                                           (- maxy miny)))))
             (_ (update-bounding-rect))
             (selected-id
              (add-hook
               'frame
               (→ (for-each
                   (→1 (draw-line
                        (cons (+ ∆x (caar x)) (+ ∆y (cdar x)))
                        (cons (+ ∆x (car (cadr x))) (+ ∆y (cdr (cadr x))))
                        2 (aq 'selected *colorscheme*)))
                   sel-mirrors))))
             (b-rect-id (add-hook 'frame (→ (gui/rect bounding-rect (aq 'red *colorscheme*)))))
             (cursor-handler-id
              (add-hook
               'frame
               (→ (if (and (not menu-open) (point-in-rect? (get-mouse-position) bounding-rect))
                      (set-cursor MOUSE-CURSOR-RESIZE-ALL)
                      (set-cursor MOUSE-CURSOR-ARROW)))))
             (move-handler-id
              (add-hook
               'click
               (lambda (first l r)
                 (let ((mp (get-mouse-position)))
                   (when (not menu-open)
                     (when l
                       (when first
                         (set! ∆mouse (cons (- (car mp) minx ∆x)
                                            (- (cdr mp) miny ∆y))))
                       (set! ∆x (- (car mp) minx (car ∆mouse)))
                       (set! ∆y (- (cdr mp) miny (cdr ∆mouse)))
                       (update-bounding-rect)))))))
             (menu-handler-id
              (add-hook
               'click
               (lambda (first l r)
                 (when (and first r (not menu-open))
                   (set! menu-open #t)
                   (set! *gui/option-menu-force-can-be-handled* #t)
                   (gui/option-menu
                    (get-mouse-position)
                    `(("usuń" . ,(→ (set! menu-open #f)
                                    (for-each
                                     delete-bounceable
                                     sel-mode:selected-mirror-ids)
                                    (end-selected-mode))))
                    (→ (set! menu-open #f)))))))
             (end-selected-mode
              (→(delete-hook 'frame selected-id)
                 (delete-hook 'frame b-rect-id)
                 (delete-hook 'frame cursor-handler-id)
                 (delete-hook 'click move-handler-id)
                 (delete-hook 'click menu-handler-id)
                 (delete-hook 'click close-handler-id)
                 (really-end-selected-mode)))
             (close-handler-id
              (add-hook
               'click
               (lambda (first l r)
                 (when (not menu-open)
                   (when (and first l (not (point-in-rect? (get-mouse-position) bounding-rect)))
                     (for-each
                      (→1 (let ((p1 (list-ref x 1))
                                (p2 (list-ref x 2)))
                            (set-mirror!
                             (car x)
                             (cons (+ ∆x (car p1)) (+ ∆y (cdr p1)))
                             (cons (+ ∆x (car p2)) (+ ∆y (cdr p2))))))
                      real-sel-mirrors)

                     (end-selected-mode)))))))
             0)
        (really-end-selected-mode)))

(define (really-end-selected-mode)
  (set! *gui/option-menu-force-can-be-handled* #f)
  (set! sel-mode:last-time
        (if (> (length sel-mode:selected-mirror-ids) 0)
            (time)
            0))
  (set! *click-can-be-handled* #t)
  (set! *current-mode* nil)

  (start-simulation))

;;; mirror data
(define *mirrors* nil)
(define (reload-*mirrors*)
  (let ((bbs (get-all-bounceables)))
    (set!
     *mirrors*
     (map
      (→1 (cons (car x) (cdr (cdr x))))
      (filter
       (→1 (if (not (eqv? (cdr x) #f))
               (if (eqv? (cadr x) 'mirror)
                   #t
                   #f)
               #f))
       (map
        (→1 (append (list x) (list-ref bbs x)))
        (⍳ 0 1 (length bbs))))))))

(define (update-*mirrors* id)
  (set!
   *mirrors*
   (map
    (→1 (if (eqv? (car x) id)
            (append (list id) (cdr (get-bounceable id)))
            x))
    *mirrors*)))

(define (new-mirror-update-*mirrors* id)
  (set!
   *mirrors*
   (append *mirrors* (list (append (list id) (cdr (get-bounceable id)))))))

(add-hook
 'new
 (→2 (cond
      ((eqv? x 'mirror) (new-mirror-update-*mirrors* y)) ; TODO: wymysl lepsza nazwe + idk czy reload-*mirrors* bedzie kiedykolwiek potrzebne
      (else (error "not implemented: " x)))))

(add-hook
 'update
 (→2 (cond
      ((eqv? x 'mirror) (update-*mirrors* y))
      (else (error "not implemented: " x)))))

(add-hook
 'delete
 (→2 (cond
      ((eqv? x 'mirror)
       (set!
        *mirrors*
        (filter
         (→1 (not (eqv? (car x) y)))
         *mirrors*)))
      (else (error "not implemented: " x)))))

(add-hook
 'frame
 (→ (draw-text
     (string-append
      (if (null? *current-mode*)
          "normal"
          (symbol->string *current-mode*))
      "-mode")
     (cons 16 (- *SCREEN-HEIGHT* 32)) 16 (aq 'font *colorscheme*))))
