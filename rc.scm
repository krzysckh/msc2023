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
     ((eqv? k #\+)
      (set! current-thickness (min (+ current-thickness 1) max-thickness))
      (update-thicknesses))
     ((eqv? k #\-)
      (set! current-thickness (max (- current-thickness 1) 1))
      (update-thicknesses)))))

(add-hook 'keypress kp-hook)


(define gui/slider:ident 'GUI-slider)
;; TODO: sposób stwierdzenia jak długo ma istnieć i posprzątania po nim
(define (gui/slider rect from to cb)
  "tworzy slider. wywołuje `cb` z wynikiem za każdym `'unclick` eventem"
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
                 `(,current-x ,(list-ref rect 1) ,slider-rect-w ,(list-ref rect 3))))))
    (update-slider-rect)
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
                (set! *click-can-be-handled* #t))))))

    (add-hook
     'click
     (lambda (first l r)
       (let ((mp (get-mouse-position)))
         (when (or (and (or (point-in-rect? mp inner-rect) (point-in-rect? mp slider-rect))
                        (or *click-can-be-handled* (eqv? *current-click-handler*
                                                         gui/slider:ident))
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
             (cb r))))))

    (add-hook
     'unclick
     (→3 (when (eqv? *current-click-handler* gui/slider:ident)
           (set! holding #f)
           (set! *current-click-handler* nil)
           (set! *click-can-be-handled* #t))))))

(define current-slider-state 0)
;; TODO: ten slider cos mi kurrrcze cuchnie
;; (gui/slider '(100 100 100 20) 0 100 (→1 (set! current-slider-state x)))
;; (add-hook
;;  'frame
;;  (→ (draw-text (number->string current-slider-state)
;;                '(210 . 100)
;;                16
;;                (aq 'font *colorscheme*)
;;                3)))

;; to jest niesamowite ze to dziala
;; ~ kpm
(letrec ((d (car (gui/btn '(100 . 100) "kliknij mnie" (→ (d))))))
  nil)
