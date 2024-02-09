# co, gdzie i jak

Główna część programu napisana jest w C, jednakże by ułatwić i usprawnić proces
tworzenia, wbudowałem do niej [interpreter scheme](https://tinyscheme.sourceforge.net/).

Każdy nowy plik w folderze `scm/` z rozszerzeniem `.scm` będzie linkowany
z programem i uruchamiany na samym początku. Te pliki służą do definiowania
"systemowych" funkcji, a więc (w przyszłości) UI, wszystkiego związanego
z przemieszczaniem obiektów na ekranie, etc.

Rdzeń myślący i obliczający rzeczy zostanie jednak w C.

# jak korzystać z scheme?

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
- `'frame` - wykonywany co klatkę. ***UWAGA: bardzo spowalnia mainloop. po wykorzystaniu należy go usunąć***

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

# dokumentacja pre-definiowanych funkcji dla scheme:
