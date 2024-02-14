;;-- już-zrobione przykłady które można załadować

(define *examples* '())
(define *current-example* nil)

(define (e:delete-all)
  (delete-all-sources)
  (for-each
   delete-bounceable
   (map car (append *mirrors* *customs* *prisms*))))

(define (load-example n)
  ((cdr (list-ref *examples* n))))

(define (define-example nam user-f)
  (let ((f (→ (e:delete-all)
              (user-f))))
    (set! *examples* (append *examples* (list (cons nam f))))))

(define-example "źródło i pryzmat"
  (→ (create-prism '(305 . 326) 100.0 1.309999943)
     (create-prism '(552 . 364) 203.0 1.309999943)
     (create-source '((pos 88 . 359) (angle . 353) (thickness . 1) (reactive . #f) (n-beams . 3) (color 214 153 182)))))
