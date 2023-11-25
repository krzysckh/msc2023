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

(define (create-source a)
  (let ((x (aq-or 'x a 100))
        (y (aq-or 'y a 100))
        (size (aq-or 'size a 20))
        (ang (aq-or 'angle a 90))
        (thickness (aq-or 'thickness a 1))
        (reactive (aq-or 'reactive a #t)))
    (real-create-source x y size ang thickness reactive)))
