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

(define (rand-float)
  (/ (random-next) 2147483647))

;; (define (px pt)
;;   (add-mirror pt (cons (+ 2 (car pt)) (+ 2 (cdr pt)))))

;; lol
;; (define-example "fern"
;;   (→ (let ((max-iter 4096)
;;            (x 0.0)
;;            (y 0.0)
;;            (xn 0.0)
;;            (yn 0.0))
;;        (for-each
;;         (lambda (n) (let ((r (rand-float)))
;;                  (cond
;;                   ((< r 0.01)
;;                    (set! xn 0.0)
;;                    (set! yn (* 0.16 y)))
;;                   ((< r 0.86)
;;                    (set! xn (+ (* 0.85 x) (* 0.04 y)))
;;                    (set! yn (+ (* -0.04 x) (* 0.85 y) 1.6)))
;;                   ((< r 0.93)
;;                    (set! xn (- (* 0.2 x) (* 0.26 y)))
;;                    (set! yn (+ (* 0.23 x) (* 0.22 y) 1.6)))
;;                   (else
;;                    (set! xn (+ (* -0.15 x) (* 0.28 y)))
;;                    (set! yn (+ (* 0.26 x) (* 0.24 y) 0.44))))
;;                  (let ((xr (* (/ (+ xn 3) 6) *SCREEN-WIDTH*))
;;                        (yr (* (/ yn 10) *SCREEN-HEIGHT*)))
;;                    (px (cons xr yr)))
;;                  (set! x xn)
;;                  (set! y yn)))
;;         (⍳ 0 1 max-iter)))))
