``` shell
$ mypaint ./notatki.ora
$ make
$ ./main

$ make build-windows
$ wine ./main.exe
```

## uwagi

-   nie edytuj README.md - jest generowane automatycznie z README.org
-   testowane na raylib 4.5. wyższe werje mogą nie działać bo coś tam
    odwalili z liczeniem wektorów lololol

``` shell
$ git clone https://github.com/raysan5/raylib
$ cd raylib
$ git checkout 4.5.0
$ cd src/
$ make
$ sudo make install
```

-   wymaga raygui
-   wymaga **GNU Make** (bsd make ma problem z *.SUFFIXES*)
-   wzory i pomysły są wyjęte z dupy, np.:

$$ H = \text{wysokość okna} $$

$$
\operatorname{target}(a, k) = \begin{cases}
\begin{bmatrix}
  ctg(\frac{\pi}{180}(180-k))y_{a}+x_{a} \\
  0
\end{bmatrix} & \text{ jeśli } k \in \left\langle 180, 360 \right\rangle \cup \left\{ 0 \right\}
\\
\begin{bmatrix}
  x_{a}+\frac{(H-y_{a})}{ctg((90-k)\frac{\pi}{180})} \\
  H
\end{bmatrix} & \text{ w innych wypadkach }
\end{cases}
$$

w notatkach oczywiscie cos tam jest skad one sie biora, ale sa zupelnie
nie po kolei i nawet sam sie juz po nich nie doczytam lolololol

Piszę tą próbę w C, bo nie znalazłem żadnej dobrej biblioteki do pisania
\"gier\" dla racketa. Oczywiście jest wrapper dla raylib, ale dużo
szybciej dla mnie jest napisać to najpierw w C + raylib a potem dopiero
(jeśli w ogóle) próbować przeportować to do racketowego wrappera dla
raylib.

Jest to bardzo nieokiełznany **proof-of-concept**, który musi zostać
opatrzony w wiele wiecej struktur itp itd, żeby przypominał jakkolwiek
to, o co prosi zadanie. Ma na celu sprawdzenie jak wyglądałoby pisanie
tego w C + raylib.

## scheme

**MIMO** tego że program jest napisany w C, możliwe jest skryptowanie
wszystkiego w tinyscheme, bo bardzo łatwy jest *interop* między C a
tinyscheme, co widać tu o.

pre-kompiluje i uruchamia od razu wszystko w `scm/`

dokumentacja funkcji scheme będzie/jest dostępna
[tutaj](https://pub.krzysckh.org/msc2023.html) (jeszcze nie) lub może
być zbudowana używając `make scheme-docs` (wymaga perla i sporej ilości
modułów)

Zimplementowane funkcje:

-   `(measure-text text sz . spacing)`

-   `(draw-text text pos sz color . spacing)`

-   `(get-all-sources)` zwraca wszystkie źródła. lepiej używać
    `*sources*`, bo ma zawsze takie same wartości, a jest zmienną a nie
    funkcją.

-   `(get-source n)` zwraca dane o źródle
    `(x y sz angle thickness r g b a)`

-   `(set-source! n x y ang thickness r g b a)` - aktualizuje źródło n,
    leiej używać `(set-source-e!)`

-   `(set-source-e! n sym v)` aktualizuje jedną rzez dla źródła *n*,
    `sym` to `'pos | 'angle | 'thickness | 'color`. v to wartość, dla
    `'pos` `(x . y)`, dla `'color` `(r g b a)`

-   `(create-mirror x1 y1 x2 y2)` tworzy zwierciadło (jak na razie bez
    typu etc).

-   `(create-source alist)` tworzy `source_t`

-   `(add-hook h f)` dodaje hook `f` do akcji *h*

-   `(get-mouse-position)` zwraca `(x . y)` myszki

-   `(draw-line p1 p2 thickness color)` rysuje linię. p1 i p2 =
    `(x . y)`, color = `(r g b a)`

-   na początek odpala `./rc.scm`

## keybindingi

-   *A* - dodaje sztynks
-   *e* - pokazuje okno wykonywania scheme (do poprawki lolololol)

## odniesienia

-   [zadanie](https://science-cup.pl/wp-content/uploads/2023/11/MSC4_2023_Optyka.pdf)
