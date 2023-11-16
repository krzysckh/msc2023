#lang racket

(require racket/gui/easy
         racket/gui/easy/operator
         racket/math
         (prefix-in G: racket/gui))

(struct HSV (h s v))
(struct RGB (r g b))

; zwięźlejsze. pewnie lepiej zmienić tego nazwę na coś bez ? na końcu,
; bo to nie pytanie o typ
(define @? obs-peek)

; zmienne zmienne
(define @hsv (@ (HSV 0 0 0)))
(define @cur-color (@ (G:make-color 0 0 0)))

(define (print s . v)
  (display s)
  (for-each (lambda (x) (display " ") (display x)) v)
  (newline))

; TODO szczerze nie mam pojecia czy robi to poprawnie
; i to jest do sprawdzenia
; https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB
(define (HSV->RGB h)
  (let* ((H (HSV-h h))
         (S (/ (HSV-s h) 100))
         (V (/ (HSV-v h) 100))
         (C (* S V))
         (Hp (/ H 60))
         (X (* C (- 1 (abs (- (modulo (floor Hp) 2) 1)))))
         (m (- V C))
         (RGB1 (cond
                 ((and (<= 0 Hp) (< Hp 1)) (RGB C X 0))
                 ((and (<= 1 Hp) (< Hp 2)) (RGB X C 0))
                 ((and (<= 2 Hp) (< Hp 3)) (RGB 0 C X))
                 ((and (<= 3 Hp) (< Hp 4)) (RGB 0 X C))
                 ((and (<= 4 Hp) (< Hp 5)) (RGB X 0 C))
                 ((and (<= 5 Hp) (< Hp 6)) (RGB C 0 X))
                 (else
                   (RGB 0 0 0))))) ; idk lol podobno nie powinno sie wydarzyc
    (RGB
      (exact-round (* 255 (+ (RGB-r RGB1) m)))
      (exact-round (* 255 (+ (RGB-g RGB1) m)))
      (exact-round (* 255 (+ (RGB-b RGB1) m))))))

; nie podoba mi sie jak malo funkcyjne sa (obs x)
; ale no cos za cos, easy-gui bardzo pomaga
(define (update-cur-color)
  (let* ((rgb (HSV->RGB (@? @hsv)))
         (r (RGB-r rgb))
         (g (RGB-g rgb))
         (b (RGB-b rgb)))
    (print "nowy kolor: " (list r g b))
    (<~ @cur-color (lambda (_) (G:make-color r g b)))))

(render
  (window
    #:title "lambda-kolory"
    #:position 'center
    (text (~> (~> @hsv HSV-h) number->string))
    (text (~> (~> @hsv HSV-s) number->string))
    (text (~> (~> @hsv HSV-v) number->string))

    (text "GIGA TEKST"
          #:color @cur-color
          #:font (font "monospace" 20))

    (slider
      (~> @hsv HSV-h)
      (lambda (x)
        (<~ @hsv (lambda (v) (HSV x (HSV-s v) (HSV-v v))))
        (update-cur-color))

      #:min-value 0
      #:max-value 360)

    (slider
      (~> @hsv HSV-s)
      (lambda (x)
        (<~ @hsv (lambda (v) (HSV (HSV-h v) x (HSV-v v))))
        (update-cur-color))
      #:min-value 0
      #:max-value 100)

    (slider
      (~> @hsv HSV-v)
      (lambda (x)
        (<~ @hsv (lambda (v) (HSV (HSV-h v) (HSV-s v) x)))
        (update-cur-color))
      #:min-value 0
      #:max-value 100)))
