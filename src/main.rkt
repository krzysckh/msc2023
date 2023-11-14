#lang racket

(require racket/gui/easy
         racket/gui/easy/operator
         racket/base
         racket/math
         (prefix-in G: racket/gui))

(define (print s . v)
  (display s)
  (for-each (λ (x) (display " ") (display x)) v)
  (newline))

; https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB
(define (HSV->RGB h)
  (let* ((H (HSV-h h))
         (S (exact->inexact (/ (HSV-s h) 100)))
         (V (exact->inexact (/ (HSV-v h) 100)))
         (C (exact->inexact (* S V)))
         (Hp (exact->inexact (/ H 60)))
         (X (exact->inexact
              (* C (- 1 (abs (- (modulo (floor (exact->inexact Hp)) 2) 1))))))
         (m (- V C))
         (RGB1 (cond
                 ((and (<= 0 Hp) (< Hp 1)) (RGB C X 0))
                 ((and (<= 1 Hp) (< Hp 2)) (RGB X C 0))
                 ((and (<= 2 Hp) (< Hp 3)) (RGB 0 C X))
                 ((and (<= 3 Hp) (< Hp 4)) (RGB 0 X C))
                 ((and (<= 4 Hp) (< Hp 5)) (RGB X 0 C))
                 ((and (<= 5 Hp) (< Hp 6)) (RGB C 0 X)))))
    (RGB
      (exact-round (* 100 (+ (RGB-r RGB1) m)))
      (exact-round (* 100 (+ (RGB-g RGB1) m)))
      (exact-round (* 100 (+ (RGB-b RGB1) m))))))

(struct HSV (h s v))
(struct RGB (r g b))

(define @? obs-peek)

(define @hsv (@ (HSV 0 0 0)))
(define @cur-color (@ (G:make-color 0 0 0)))

(define (update-cur-color)
  (let* ((rgb (HSV->RGB (@? @hsv)))
         (r (RGB-r rgb))
         (g (RGB-g rgb))
         (b (RGB-b rgb)))
    (print (list r g b))
    (<~ @cur-color (λ (_) (G:make-color r g b)))))

(define r
(render
  (window
    #:title "λ-kolory"
    #:position 'center
    (text ((@hsv . ~> . HSV-h) . ~> . number->string))
    (text ((@hsv . ~> . HSV-s) . ~> . number->string))
    (text ((@hsv . ~> . HSV-v) . ~> . number->string))

    (text "GIGA TEKST"
          #:color @cur-color
          #:font (font "monospace" 20))

    (slider
      (@hsv . ~> . HSV-h)
      (λ (x)
        (<~ @hsv (λ (v) (HSV x (HSV-s v) (HSV-v v))))
        (update-cur-color))

      #:min-value 0
      #:max-value 360)

    (slider
      (@hsv . ~> . HSV-s)
      (λ (x)
        (<~ @hsv (λ (v) (HSV (HSV-h v) x (HSV-v v))))
        (update-cur-color))
      #:min-value 0
      #:max-value 100)

    (slider
      (@hsv . ~> . HSV-v)
      (λ (x)
        (<~ @hsv (λ (v) (HSV (HSV-h v) (HSV-s v) x)))
        (update-cur-color))
      #:min-value 0
      #:max-value 100)))

)
