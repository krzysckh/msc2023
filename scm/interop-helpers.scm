(define *sources* '())

; TODO: 'resize hook
(define *SCREEN-SIZE* (get-screen-size))
(define *SCREEN-WIDTH* (car *SCREEN-SIZE*))
(define *SCREEN-HEIGHT* (cdr *SCREEN-SIZE*))

(define (aq e alist)
  "zwraca wynik assq bez car"
  (args
   '((e . "element szukany")
     (alist . "lista asocjasyjna")))
  (cdr (assq e alist)))

(define (cdr* l)
  "zwraca cdr dla l jeśli l to lista lub para"
  (if (or (pair? l) (list? l))
    (cdr l)
    l))

(define (aq-or e alist o)
  "zwraca wynik (assq e alist) jeśli e istnieje w alist. w przeciwnym wypadku o"
  (let ((v (assq e alist)))
    (if v
      (cdr v)
      o)))

(define (update-sources)
  "wewnętrzna funkcja aktualizująca *sources* za każdym razem gdy zostaną
  zmienione"
  (set! *sources* (get-all-sources)))

(define (create-source a)
  "tworzy nowe source_t (źródło światła)"

  (args
   '((a . "lista asocjacyjna z elementami 'x, 'y, 'size, 'angle, 'thickness,
           'reactive, 'color, 'n-beam.
           wszystkie elementy mają wartości domyślne i mogą być pominięte.
           zamiast 'x i 'y, może zostać zdefiniowane samo 'pos")))
  (example
   '((create-source '((x . 500) (y . 500) (reactive . #t)))
     "tworzy reagujące na myszkę źródło na pozycji (500 500)"))

  (let* ((pos (aq-or 'pos a `(,(aq-or 'x a 100) . ,(aq-or 'y a 100))))
         (x (car pos))
         (y (cdr pos))
         (size (aq-or 'size a 20))
         (ang (aq-or 'angle a 90))
         (thickness (aq-or 'thickness a 1))
         (reactive (aq-or 'reactive a #t))
         (n-beams (aq-or 'n-beams a 1))
         (color (aq-or 'color a (aq 'default-light *colorscheme*))))
    (real-create-source x y size ang thickness reactive n-beams color))
  (update-sources))

(define (draw-line pt1 pt2 thick color)
  "rysuje linię od pt1 do pt2 o grubości thick i kolorze color"
  (args
   '((pt1 . "punkt 1 (x . y)")
     (pt2 . "punkt 2 (x . y)")
     (thick . "grubość")
     (color . "kolor (r g b a)")))

  (let ((x1 (car pt1))
        (y1 (cdr pt1))
        (x2 (car pt2))
        (y2 (cdr pt2)))
    (real-draw-line x1 y1 x2 y2 thick color)))

(define (set-source! n x y ang thickness mouse-reactive n-beams color)
  "aktualizuje źródło o id n ustawiając wszystkie jego wartości. lepiej używać set-source-e!"
  (real-set-source! n x y ang thickness mouse-reactive n-beams color)
  (update-sources))

(define (set-source-e! n sym v)
  "aktualizuje właściwość sym na v w źródle o id n"
  (args
   '((n . "id źródła")
     (sym . "'pos | 'angle | 'thickness | 'color | 'mouse-reactive | 'n-beams w zależności od tego co chcemy zmienić")
     (v . "nowa wartość dla sym")))

  (if (> n (length *sources*))
    #f
    (let* ((src (get-source n))
           (x (if (eq? 'pos sym) (car v) (car (list-ref src 0))))
           (y (if (eq? 'pos sym) (cdr v) (cdr (list-ref src 0))))
           (ang (if (eq? 'angle sym) v (list-ref src 1)))
           (thickness (if (eq? 'thickness sym) v (list-ref src 2)))
           (mouse-reactive (if (eq? 'mouse-reactive sym) v (list-ref src 3)))
           (n-beams (if (eq? 'n-beams sym) v (list-ref src 4)))
           (color (if (eq? 'color sym) v (list-ref src 5))))
      (set-source! n x y ang thickness mouse-reactive n-beams color)))
  (update-sources))

(define *default-spacing* 4)

; (measure-text text size . spacing) → (w . h)
(define (measure-text text size . spacing)
  "zwraca (w . h) tekstu text o wielkości size i spacingu spacing (jeśli podany)"
  (let ((spacing (if (null? spacing) *default-spacing* spacing)))
    (real-measure-text text size spacing)))

; (real-draw-text text x y sz spacing color) → #t
(define (draw-text text pos sz color . spacing)
  "wypisuje tekst text domyślnym fontem na pozycji pos, o wielkości sz i kolorze
  color. można też podać spacing."
  (args '((color . "w postaci (r g b a)")))
  (let ((x (car pos))
        (y (cdr pos))
        (spc (if (null? spacing) *default-spacing* spacing)))
    (real-draw-text text x y sz spc color)))

(define add-system-hook real-add-hook)

;; to jest takie idiotyczne XDDD
;; GC Tinyscheme usuwa lambdy przekazywane do FF funkcji (tych zdefiniowanych w c)
;; dlatego żeby cały czas o nich "pamiętało" dodaje je do *all-hooks* :33333
;; ~ kpm
(define *all-hooks* '())

;; TODO: zrobić tak żeby nie trzeba było sprawdzać *can-click-be-handled* etc.
;; ~ kpm
(define (add-user-hook s f)
  "w przyszłości będzie dodawała hooki które mogą być blokowane przez systemowe"
  (set! *all-hooks* (append *all-hooks* (list f)))
  (real-add-hook s (last *all-hooks*)))

(define add-hook add-user-hook)
