# nazewnictwo

Lista rozwiewająca wątpliwości co do nazewnictwa w tym dokumencie.

- <kbd> RET </kbd> - przycisk *"Enter"* / *"Return"* na klawiaturze
- RMB - prawy przycisk myszy
- LMB - lewy przycisk myszi
- scheme - lispo-podobny język programowania wbudowany w program.
- `bounceable_t` - typ zapisany w języku C, który opisuje dany obiekt, z którym wiązka może się spotkać.
- implementacja (dla funkcji zdefiniowanych dla scheme) (kod pod sygnaturą funkcji) - mało ważna informacja zawierająca kod implemenujący daną funkcję.
- `document-function` - sposób dokumentowania funkcji, które zdefiniowane zostały w C.

# co, gdzie i jak

[Ten plik może być przeglądany jako dokument HTML.](https://pub.krzysckh.org/msc2023.html)


Główna część programu napisana jest w C, jednakże by ułatwić i usprawnić proces
tworzenia, wbudowałem do niej [interpreter scheme](https://tinyscheme.sourceforge.net/).

Każdy nowy plik w folderze `scm/` z rozszerzeniem `.scm` będzie linkowany
z programem i uruchamiany na samym początku. Te pliki służą do definiowania
"systemowych" funkcji, a więc (w przyszłości) UI, wszystkiego związanego
z przemieszczaniem obiektów na ekranie, etc.

Rdzeń myślący i obliczający rzeczy zostanie jednak w C.

# przykłady

Aktualną "scenę" można zapisać przez menu (RMB → `zapisz scenę do pliku` → `nazwa-pliku.scm` <kbd> RET </kbd>), lub
wykonując funkcję `(serialize:save-to file-name)`.

Wbudowane jest też kilka przykładów (zapisane w `scm/e.scm`), można je przeglądać przez menu (RMB → załaduj przykład).

# jak korzystać ze scheme?

Pliki `*.scm` można "ładować" po prostu przerzucając je na działający program - jest to równoważne do `(load file-path)`.

Proste wyrażenia scheme można wykonywać poprzez kliknięcie klawisze <kbd> e </kbd>, po wpisaniu wyrażenia wystarczy kliknąć <kbd> RET </kbd> i zostanie ono
wyewaluowane.


cały zamysł opiera się na pomyśle *hooków* (inaczej eventów), nazwę zaciągnąłem prosto z gnu emacs.
o danym hooku należy myśleć jak o evencie w JS na stronie internetowej.
np.:

```javascript
const el = document.getElementById("btn");
btn.addEventListener('click', (e) => {
  ...
});
```

tutaj wygląda tak:

```scheme
(add-hook
 'click
 (lambda (first l r)
   ...))
```

jest wiele różnych rodzajów hooków o które można się *"zaczepić"*. pełna lista jest w `src/scheme-interop.c`.

- `'keypress` - wykonywany za każdym razem gdy klawisz na klawiaturze jest wciśnięty.
                jako argumenty przekazuje wciśnięty znak jako `char`, oraz jego numeryczną
                wartość (dla nie-ascii znaków)
- `'click` - wykonywany na każdej klatce, podczas której wciśnięty jest przycisk myszy.
             jako argumenty przekazuje `bool czy_pierwsze_przycisniecie`, `bool czy_lewy`
             i `bool czy_prawy`.
- `'unclick` - wykonywany podczas klatki w której przycisk myszy przestanie być wciskany
               przekazuje takie same argumenty jak `'click`
- `'resize` - wykonywany za każdym razem jak rozmiar okna zostanie zmieniony.
              przekazuje aktualną szerokość i wysokość okna.
- `'clocke` - wykonywany co sekundę. przekazuje aktualny czas.
- `'loge` - wykonywany za każdym `TraceLog()`iem z C, bądź `(tracelog)`iem z scheme.
            przekazuje typ logu i string z logiem.
- `'new` - wykonywany po stworzeniu nowego `bounceable_t` - obiektu od którego światło może
           się odbić. przekazuje typ stworzonego `bounceable` (jako symbol).
- `'update` - wykonywany za każdym razem, gdy dane `bounceable` zostało edytowane.
              przekazuje typ (j.w.) i `id`.
- `'delete` - wykonywany za każdym razem, gdy dane `bounceable` zostało usunięte.
              przekazuje typ (j.w.) i `id`.
- `'files-dropped` - wykonywany, gdy pliki zostały "wrzucone" do okna (przeniesione z innego programu - 
                     drag&drop)
- `'frame` - wykonywany co klatkę. ***UWAGA: spowalnia mainloop. po wykorzystaniu należy go usunąć***

hooki dodaje się (j.w.) funkcją `add-hook`. zwraca ona `id` danego hooka, po którym
można go potem usunąć.
np.:

```scheme
(define id
        (add-hook
         'frame
         (lambda ()
           (draw-text "halo" '(10 . 10) 16 white))))

(wait 2 (lambda () (delete-hook 'frame id)))
```

przez dwie sekundy będzie wyświetlać `halo` w {x: 10, y: 10}.

# "własne obiekty o dowolnym kształcie"

tworzenie "własnych obiektów o dowolnym kształcie" odbywa się poprzez zdefiniowanie ich w języku scheme.
są uznawane za normalne `bounceable_t` typu `B_CUSTOM`.

wytłumaczę poprzez przykłady:

- chcę stworzyć **deltoid**, od którego wiązki światła odbijają się jak od normalnych zwierciadeł.

```scheme
(define p1 '(100 . 100))
(define p2 '(150 . 150))
(define p3 '(100 . 400))
(define p4 '(50  . 150))
(define punkty-deltoidu (list p1 p2 p3 p4))

;; L[1-4] to linie w deltoidzie

(define L1 (list p1 p2))
(define L2 (list p2 p3))
(define L3 (list p3 p4))
(define L4 (list p4 p1))

;;      p1
;;      .
;; L4  / \   L1
;; p4 .   .  p2
;;    \   /
;; L3  \ /   L2
;;      v
;;     p3

;; punkty-deltoidu to wielokąt (poly w customb_data_t), czyli lista punktów.
;; jeśli znajdzie się w niej wiązka światła, program wykona funkcję, która obliczyć ma jak światło powinno się odbić.
;; w tym przypadku będzie to funkcja bounce-fn. dostaje ona jako dane punkt w którym wiązka światła dotknęła wielokątu,
;; oraz kąt względem osi OX
(define (bounce-fn hit-point angle)
  (let* ((hit-line
          (cond
           ((point-in-line? hit-point (car L1) (cadr L1) 2) L1) ;; na początek sprawdzamy o jaką linię deltoidu
           ((point-in-line? hit-point (car L2) (cadr L2) 2) L2) ;; wiązka światła faktycznie się odbiła i zapisujemy ją
           ((point-in-line? hit-point (car L3) (cadr L3) 2) L3) ;; jako hit-line
           ((point-in-line? hit-point (car L4) (cadr L4) 2) L4)
           (else
            (error "not in-line"))))
         (hit-angle (normalize-angle (angle-between (car hit-line) (cadr hit-line)))) ;; kąt pod jakim jest linia deltoidu
         (rel-angle (- hit-angle angle))                                              ;; kąt pod jakim światło padło na deltoid
         (next-angle (normalize-angle (+ hit-angle rel-angle))))                      ;; kąt jaki teraz ma obrać światło
    (list hit-point next-angle)))
;;        ^^^^^^^^^ ^^^^^^^^^^
;;        \____               \_____
;;             \                    \
;; zwrócić mamy to samo co dostaliśmy, t.j. punkt od którego ma wiązka kontynuować, oraz kąt (względem osi OX)

;; musimy oczywiście też zdefiniować funkcję, która pokaże gdzie ten deltoid jest (t.j. narysuje go)
;; funkcja ta będzie wywoływana za każdym razem gdy klatka ma być narysowana (często) więc powinna być jak najkrótsza.
(define kolor-deltoidu lime-green) ; via scm/colors.scm
(define (draw-fn)
  (draw-line p1 p2 1 kolor-deltoidu)
  (draw-line p2 p3 1 kolor-deltoidu)
  (draw-line p3 p4 1 kolor-deltoidu)
  (draw-line p4 p1 1 kolor-deltoidu))

;; na koniec musimy "zarejestrować" deltoid, t.j. przekazać wszystkie informacje o nim programowi
(register-custom
 punkty-deltoidu
 draw-fn
 bounce-fn)
```

- chcę stworzyć portal, który przeniesie wiązkę do innego portalu o takich samych wymiarach

```scheme
;; początek i koniec portalu mają takie same wymiary i są równoległe do osi OY,
;; żeby można było łatwo policzyć gdzie światło ma sie znaleźć
(define portal-start '((500 . 500) (500 . 700)))
(define portal-end   '((400 . 100) (400 . 200)))

;; portal początkowy rysowany jest kolorem zielonym, a końcowy czerwonym
(define (draw-fn)
  (draw-line (car portal-start) (cadr portal-start) 1 green)
  (draw-line (car portal-end) (cadr portal-end) 1 red))

(define (light-remap-fn hit-point angle)
  (let* ((hit-y (cdr hit-point))
         (diff-y (- hit-y (cdr (car portal-start)))) ;; różnica między początkiem (górą) portalu, a miejscem, gdzie wiązka go dotknęła
         (end-y (+ (cdr (car portal-end)) diff-y)))  ;; finalne y, w którym pojawić ma się wiązka
    (list (cons (caar portal-end) end-y) angle)))
;;              ^^^^^^^^^^^^^^^^  ^^^^^
;;        x portalu końcowego i (y portalu końcowego + diff-y)

(register-custom
 portal-start
 draw-fn
 light-remap-fn)
```

# dokumentacja pre-definiowanych funkcji dla scheme:
