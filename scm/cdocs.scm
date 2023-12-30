;; tu jest dokumentacja funkcji zdefiniowanych w src/scheme-interop.c
(macro (document-function v) '())

(document-function
 (time-since-init)
 "zwraca ile czasu minęło od początku działania programu (wg. raylib - od `InitWindow()`)")

(document-function
 (time)
 "zwraca aktualny unix timestamp")

(document-function
 (system s)
 "wykonuje `sh -c $s` i zwraca stdout")

(document-function
 (exit . status)
 "kończy program. zwraca `status` jeśli podany, inaczej 0")

(document-function
 (loads s)
 "wykonuje `s` (to samo co eval, tylko że nie zwraca wartości i akceptuje string, nie sexp)")

(document-function
 (delete-hook sym n)
 "usuwa hook dla `sym` o id `n`"
 (args
  '((sym . "`hookable_event_t` via src/scheme-interop.c")
    (n . "id zwrócone przez `add-hook`"))))

(document-function
 (get-source n)
 "zwraca informacje o źródle n")

(document-function
 (get-all-sources)
 "zwraca listę wszystkich źródeł")

(document-function
 (create-mirror x1 y1 x2 y2)
 "tworzy zwierciadło")

(document-function
 (get-mouse-position)
 "zwraca pozycje myszki na oknie w postaci `(x . y)`")
