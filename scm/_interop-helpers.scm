; ten plik ma _ przed nazwą, bo musi być ładowany pierwszy XDD
(define *sources* '())

(define (aq e alist)
  (cdr (assq e alist)))

(define (cdr* l)
  (if (or (pair? l) (list? l))
    (cdr l)
    l))

(define (aq-or e alist o)
  (let ((v (assq e alist)))
    (if v
      (cdr v)
      o)))

(define default-color '(255 0 255 255))

(define (update-sources)
  (set! *sources* (get-all-sources)))

(define (create-source a)
  (let* ((x (aq-or 'x a 100))
         (y (aq-or 'y a 100))
         (size (aq-or 'size a 20))
         (ang (aq-or 'angle a 90))
         (thickness (aq-or 'thickness a 1))
         (reactive (aq-or 'reactive a #t))
         (r (list-ref (aq-or 'color a default-color) 0))
         (g (list-ref (aq-or 'color a default-color) 1))
         (b (list-ref (aq-or 'color a default-color) 2))
         (a (list-ref (aq-or 'color a default-color) 3)))
    (real-create-source x y size ang thickness reactive r g b a))
  (update-sources))

(define (draw-line pt1 pt2 thick color)
  (let ((x1 (car pt1))
        (y1 (cdr pt1))
        (x2 (car pt2))
        (y2 (cdr pt2))
        (r (list-ref color 0))
        (g (list-ref color 1))
        (b (list-ref color 2))
        (a (list-ref color 3)))
    (real-draw-line x1 y1 x2 y2 thick r g b a)))

(define (set-source! n x y ang thickness r g b a)
  (real-set-source! n x y ang thickness r g b a)
  (update-sources))

; sym = pos | angle | thickness | color
(define (set-source-e! n sym v)
  (if (> n (length *sources*))
    #f
    (let* ((src (get-source n))
           (x (if (eq? 'pos sym) (car v) (car (list-ref src 0))))
           (y (if (eq? 'pos sym) (cdr v) (cdr (list-ref src 0))))
           (ang (if (eq? 'angle sym) v (list-ref src 1)))
           (thickness (if (eq? 'thickness sym) v (list-ref src 2)))
           (color (if (eq? 'color sym) v '(255 0 255 255)))
           (r (list-ref color 0))
           (g (list-ref color 1))
           (b (list-ref color 2))
           (a (list-ref color 3)))
      (set-source! n x y ang thickness r g b a)))
  (update-sources))

(define *default-spacing* 8)

; (measure-text text size . spacing) → (w . h)
(define (measure-text text size . spacing)
  (let ((spacing (if (null? spacing) *default-spacing* spacing)))
    (real-measure-text text size spacing)))

; (real-draw-text text x y sz spacing r g b a) → #t
(define (draw-text text pos sz color . spacing)
  (let ((x (car pos))
        (y (cdr pos))
        (r (list-ref color 0))
        (g (list-ref color 1))
        (b (list-ref color 2))
        (a (list-ref color 3))
        (spc (if (null? spacing) *default-spacing* spacing)))
    (real-draw-text text x y sz spc r g b a)))

(define add-system-hook real-add-hook)

(define (add-user-hook s f)
  (real-add-hook s f))

(define add-hook add-user-hook)
