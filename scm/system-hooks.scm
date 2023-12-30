; TODO: HACK: to powinna byc funkcja pytajaca typu (can-handle-click?)
; i ogolnie user-hooki zalezne od tego i system-hooki niezalezne
(define *click-can-be-handled* #t)
(define *keypress-can-be-handled* #t)

(define *current-click-handler* #f)
(define *current-keypress-handler* #f)

; HACK: tu o

;; ZMIANA POZYCJI DANEGO SOURCE_T
(define *source-size* 20)
(define repositioning-source #f)
(define repositioning-dx 0)
(define repositioning-dy 0)

(define (reposition-source-hook first left right)
  (when (or *click-can-be-handled* repositioning-source)
    (let ((mp (get-mouse-position)))
      (when first
        (for-each
          (lambda (n)
            (let* ((s (list-ref *sources* n))
                   (x (- (caar s) *source-size*))
                   (y (- (cdr (car s)) *source-size*)))
              (when (point-in-rect?
                      mp (list x y *source-size* *source-size*))
                (set! *click-can-be-handled* #f)
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

(define (start-drawing-mirror-hook first left right)
  (when (or *click-can-be-handled* drawing-new-mirror)
    (set! *click-can-be-handled* #f)
    (when (and first left)
      (set! drawing-new-mirror #t)
      (set! mirror-last-x (car (get-mouse-position)))
      (set! mirror-last-y (cdr (get-mouse-position))))
    (when (and (not first) drawing-new-mirror)
      (draw-line `(,mirror-last-x . ,mirror-last-y)
                 `(,(car (get-mouse-position)) . ,(cdr (get-mouse-position)))
                 2
                 '(0 0 255 255)))))

(define (end-drawing-mirror-hook first left right)
  (when drawing-new-mirror
    (set! *click-can-be-handled* #t)
    (set! drawing-new-mirror #f)
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
