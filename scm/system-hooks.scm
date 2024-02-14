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
  (create-source `((pos . ,(get-mouse-position)) (reactive . #f))))

;; DOMYŚLNE KEYBINDINGI
(define hook-status-dest nil)
(define window-opts-dest nil)
(define fps-dest nil)

(define (keypress-default-hook c _)
  (when *keypress-can-be-handled*
    (let* ((mp (get-mouse-position)))
      (cond
        ((eqv? c #\A) (create-source-at-mouse-position))
        ((eqv? c #\e) (gui/input-popup "eval scheme" loads))
        ((eqv? c #\`)
         (if (null? fps-dest)
             (begin
               (set! fps-dest (gui/show-fps '(16 . 16)))
               (set! hook-status-dest (gui/show-hook-status)))
             (begin
               (hook-status-dest)
               (fps-dest)
               (set! hook-status-dest nil)
               (set! fps-dest nil))))
        ((eqv? c #\~)
         (if (null? window-opts-dest)
             (set! window-opts-dest (gui/show-window-opts))
             (begin
               (window-opts-dest)
               (set! window-opts-dest nil))))
        ((eqv? c #\q) (exit 0))))))

(add-system-hook 'keypress keypress-default-hook)

(define (src->rect pos)
  (list (- (car pos) (/ *source-size* 2))
        (- (cdr pos) (/ *source-size* 2)) *source-size* *source-size*))

;; r-click dla źródeł
(add-hook
 'unclick
 (lambda (_ l r)
   (when (and *click-can-be-handled* r)
     (let ((mp (get-mouse-position)))
       (for-each
        (→1 (let ((pos (car (list-ref *sources* x)))
                  (cur (list-ref *sources* x)))
              (when (point-in-rect? mp (src->rect pos))
                (set! *current-mode* 'r-click-source)
                (let ((source-settings
                       `(("zmień kąt" . ,(→ (gui/mp-slider+ok
                                             0 359
                                             (lambda (v) (set-source-e! x 'angle v)) 0)))
                         ("'mouse-reactive" . ,(→ (set-source-e! x 'mouse-reactive (not (list-ref cur 3)))))
                         ("zmień ilość wiązek" . ,(→ (gui/mp-slider+ok
                                                      0 *source-size*
                                                      (lambda (v) (set-source-e! x 'n-beams v)) 0)))
                         ("usuń" . ,(→ (delete-source x))))))
                  (set! *gui/option-menu-force-can-be-handled* #t)
                  (gui/option-menu
                   (get-mouse-position)
                   source-settings
                   (→ (set! *current-mode* nil)))))))
        (⍳ 0 1 (length *sources*)))))))

;; r-click dla pryzmatów
(add-hook
 'unclick
 (lambda (_ l r)
   (when (and *click-can-be-handled* r)
     (let ((mp (get-mouse-position)))
       (for-each
        (→1 (let ((id (list-ref x 0))
                  (center (list-ref x 1))
                  (vert-len (list-ref x 5)))
              (when (point-in-triangle? mp center vert-len)
                (set! *current-mode* 'r-click-prism)
                (let ((prism-settings
                       `(("zmień współczynnik załamania pryzmatu" . ,(→ (gui/mp-slider+ok
                                                                         1.0 2.0
                                                                         (→1 (set-prism-e! id 'n x))
                                                                         3)))
                         ("zmień wielkość boku" . ,(→ (gui/mp-slider+ok
                                                       1 500
                                                       (→1 (set-prism-e! id 'vert-len x))
                                                       0))))))
                  (set! *gui/option-menu-force-can-be-handled* #t)
                  (gui/option-menu
                   (get-mouse-position)
                   prism-settings
                   (→ (set! *current-mode* nil)))))))
        *prisms*)))))

;; mouse-menu
(define mouse-menu
  `(("stwórz nowe źródło" . ,(→ (set! gui/new-source-form:pos (get-mouse-position))
                         (gui/new-source-form)))
    ("narysuj zwierciadło" . ,(→ (when (eqv? *current-mode* nil)
                                   (set-cursor MOUSE-CURSOR-CROSSHAIR)
                                   (tracelog 'info "narysuj nowe zwierciadło...")
                                   (set! *current-mode* 'mirror-drawing))))
    ("swtórz nowy pryzmat" . ,(→ (create-prism (get-mouse-position) 100 1.31)))
    ("wyrażenie scheme" . ,(→ (gui/input-popup "eval" loads)))
    ("wyczyść *tracelog-queue*" . ,(→ (set! *tracelog-queue* nil)))
    ("zapisz scenę do pliku" . ,(→ (gui/save-current)))
    ("załaduj przykład" . ,(→ (gui/load-example-menu)))))

(add-hook
 'unclick
 (lambda (first l r)
   (when (and *click-can-be-handled* r (eqv? *current-mode* nil))
     (set! *click-can-be-handled* #f)
     (set! *gui/option-menu-force-can-be-handled*)
     (gui/option-menu
      (get-mouse-position)
      mouse-menu
      (→ (set! *click-can-be-handled* #t))))))

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

;; TODO: przenieś do util.scm
(define (rect-collision? r1 r2)
  "sprawdza czy dwa `r1` i `r2` mają punkty wspólne. zwraca `#f | #t`"
  (not (eqv? (sum (rect-collision r1 r2)) 0.0)))

;; przyjmuję trójkąt równoboczny
;; :3333
(define (triangle->rect p1 p2 p3)
  (let* ((x1 (car p1))
         (y1 (cdr p1))
         (x2 (car p2))
         (y2 (cdr p2))
         (x3 (car p3))
         (y3 (cdr p3))
         (a (- (max x1 x2 x3) (min x1 x2 x3)))
         (h (* a (sqrt 3) 0.5)))
    (list (min x1 x2 x3) (min y1 y2 y3) a h)))

(define (prism->ptlist p)
  (list (list-ref p 2) (list-ref p 3) (list-ref p 4)))

(define (reposition-source-by-delta id ∆)
  (let ((pos (car (list-ref *sources* id))))
    (set-source-e! id 'pos (cons (+ (car pos) (car ∆))
                                 (+ (cdr pos) (cdr ∆))))))

(define (reposition-mirror-by-delta id ∆)
  (let* ((mirror (get-bounceable id))
         (p1 (cadr mirror))
         (p2 (caddr mirror))
         (p1-new (cons (+ (car p1) (car ∆))
                       (+ (cdr p1) (cdr ∆))))
         (p2-new (cons (+ (car p2) (car ∆))
                       (+ (cdr p2) (cdr ∆)))))
    (set-mirror! id p1-new p2-new)))

(define (reposition-prism-by-delta id ∆)
  (let* ((prism (get-bounceable id))
         (center (cadr prism))
         (center-new (cons (+ (car center) (car ∆))
                           (+ (cdr center) (cdr ∆))))
         (vert-len (list-ref prism 5))
         (n (list-ref prism 6)))
    (set-prism! id center-new vert-len n)))

(define (reposition-bounceable-by-delta id ∆)
  (let* ((thing (get-bounceable id))
         (type (car thing)))
    (cond
     ((eqv? type 'mirror) (reposition-mirror-by-delta id ∆))
     ((eqv? type 'prism) (reposition-prism-by-delta id ∆))
     (else
      (error (string-append (->string type) " unsupported"))))))

(define (thing->rect thing)
  (let ((type (car thing)))
    (cond
     ((eqv? type 'mirror) (pts->rect (cadr thing) (caddr thing)))
     ((eqv? type 'prism) (apply triangle->rect (prism->ptlist thing)))
     (else
      (error (string-append "thing->rect: unsupported" (->string thing)))))))

;;; selection-mode
(define sel-mode:start-position nil)
(define sel-mode:mirror-rects nil)
(define sel-mode:prism-rects nil)

(define sel-mode:source-rects nil)

(define sel-mode:last-time 0)
(define sel-mode:selected-bounceable-ids nil)
(define sel-mode:selected-source-ids nil)

(define (sel-mode:highlight-rects rects sel-map)
  (for-each
   (→1 (gui/rect
        (list-ref rects x)
        (if (list-ref sel-map x)
            (aq 'green *colorscheme*)
            (aq 'red *colorscheme*))))
   (⍳ 0 1 (length rects))))

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
             (set! sel-mode:source-rects (map src->rect (map car *sources*)))
             (set! sel-mode:prism-rects (map (→1 (apply triangle->rect x)) (map prism->ptlist *prisms*)))
             (set! sel-mode:mirror-rects (map (→1 (apply pts->rect x)) (map cdr *mirrors*))))
           (begin
             (let* ((mp (get-mouse-position))
                    (rect (list
                           (car sel-mode:start-position)
                           (cdr sel-mode:start-position)
                           (- (car mp) (car sel-mode:start-position))
                           (- (cdr mp) (cdr sel-mode:start-position))))
                    (selected-source-map (map (→1 (rect-collision? rect x)) sel-mode:source-rects))
                    (selected-mirror-map (map (→1 (rect-collision? rect x)) sel-mode:mirror-rects))
                    (selected-prism-map  (map (→1 (rect-collision? rect x)) sel-mode:prism-rects)))
               (sel-mode:highlight-rects sel-mode:mirror-rects selected-mirror-map)
               (sel-mode:highlight-rects sel-mode:prism-rects selected-prism-map)
               (sel-mode:highlight-rects sel-mode:source-rects selected-source-map)

               (gui/rect rect (aq 'selection *colorscheme*)))))))))

(add-hook
 'unclick
 (→3 (when (and y (eqv? *current-mode* 'selection))
       (let* ((mp (get-mouse-position))
              (sel-rect (list
                         (car sel-mode:start-position)
                         (cdr sel-mode:start-position)
                         (- (car mp) (car sel-mode:start-position))
                         (- (cdr mp) (cdr sel-mode:start-position))))
              (mirror-ids (map car (filter (→1 (rect-collision?
                                                (pts->rect (cadr x) (caddr x))
                                                sel-rect))
                                           *mirrors*)))
              (prism-ids (map car (filter (→1 (rect-collision?
                                               (apply triangle->rect (prism->ptlist x))
                                               sel-rect))
                                          *prisms*)))
              (source-ids (filter (→1 (rect-collision? sel-rect (src->rect (car (list-ref *sources* x))))) (⍳ 0 1 (length *sources*)))))
         (set! *current-mode* 'selected)
         (set! sel-mode:selected-source-ids source-ids)
         (set! sel-mode:selected-bounceable-ids (append mirror-ids prism-ids))
         (start-selected-mode)))))

;; TODO: sel custom
;; stary sposób był (chyba) trochę szybszy chociaż pewności nie mam.
;; teraz po prostu co klatkę, jeśli coś się zmieniło robię set-rzecz! bez zatrzymywania symulacji
(define (start-selected-mode)
  (if (> (+ (length sel-mode:selected-bounceable-ids)
            (length sel-mode:selected-source-ids))
            0)
      (let* ((mp (get-mouse-position))
             (menu-open #f)
             (rects (map normalize-rectangle (append (map thing->rect (map get-bounceable sel-mode:selected-bounceable-ids))
                                                     (map (→1 (src->rect (car (list-ref *sources* x)))) sel-mode:selected-source-ids))))
             (minx (minl (map car rects)))
             (miny (minl (map cadr rects)))
             (maxx (maxl (map (→1 (+ (car x) (caddr x))) rects)))
             (maxy (maxl (map (→1 (+ (cadr x) (cadddr x))) rects)))
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
                       (let ((∆last (cons ∆x ∆y)))
                         (set! ∆x (- (car mp) minx (car ∆mouse)))
                         (set! ∆y (- (cdr mp) miny (cdr ∆mouse)))
                         (for-each
                          (→1 (reposition-source-by-delta x (cons (- ∆x (car ∆last))
                                                                  (- ∆y (cdr ∆last)))))
                          sel-mode:selected-source-ids)
                         (for-each
                          (→1 (reposition-bounceable-by-delta x (cons (- ∆x (car ∆last))
                                                                      (- ∆y (cdr ∆last)))))
                          sel-mode:selected-bounceable-ids))
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
                                    (delete-sources sel-mode:selected-source-ids)
                                    (for-each delete-bounceable sel-mode:selected-bounceable-ids)
                                    (end-selected-mode))))
                    (→ (set! menu-open #f)))))))
             (end-selected-mode
              (→ (delete-hook 'frame b-rect-id)
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
                     (end-selected-mode)))))))
             0)
        (really-end-selected-mode)))

(define (really-end-selected-mode)
  (set! *gui/option-menu-force-can-be-handled* #f)
  (set! sel-mode:last-time
        (if (> (+ (length sel-mode:selected-bounceable-ids) (length sel-mode:selected-source-ids)) 0)
            (time)
            0))
  (set! *click-can-be-handled* #t)
  (set! *current-mode* nil))

;;; "toplist"y - listy z przedmiotami
(define *mirrors* nil)
(define *prisms* nil)
(define *customs* nil)

(define (update-toplist l id)
  (eval `(set! ,l (map (→1 (if (eqv? (car x) ,id)
                               (append (list ,id) (cdr (get-bounceable ,id)))
                               x))
                       ,l))))

(define (add-bounceable-to-toplist l id)
  (eval `(set! ,l (append ,l (list (append (list ,id) (cdr (get-bounceable ,id))))))))

(define (delete-from-toplist l id)
  (eval `(set! ,l (filter (→1 (not (eqv? (car x) ,id))) ,l))))

;;;----- HOOKI dla bounceable_t i *mirrors*, *prisms* etc.
;; hooki wykonywane z argumentami 'TYP ..dane
;; jako że toplisty nazywają się *TYPs*, dodaję po prostu do typu gwiazdki po obu stronach i -s na koniec
;; i mam nazwę zmiennej
;; z tąd właśnie (string->symbol (string-append "*" (symbol->string x) "s*"))
;; XDDD
;; ~ kpm

(add-hook
 'new
 (→2 (add-bounceable-to-toplist (string->symbol (string-append "*" (symbol->string x) "s*")) y)))

(add-hook
 'update
 (→2 (update-toplist (string->symbol (string-append "*" (symbol->string x) "s*")) y)))

(add-hook
 'delete
 (→2 (delete-from-toplist (string->symbol (string-append "*" (symbol->string x) "s*")) y)))

(add-hook
 'frame
 (→ (draw-text
     (string-append
      (if (null? *current-mode*)
          "normal"
          (symbol->string *current-mode*))
      "-mode")
     (cons 16 (- *SCREEN-HEIGHT* 32)) 16 (aq 'font *colorscheme*))))

;; ładowanie wrzuconych plików
(define (load-files-handler . vs)
  (for-each load vs))

(add-hook 'files-dropped load-files-handler)
