[dokumentacja scheme](https://pub.krzysckh.org/msc2023.html)
``` shell
$ mypaint ./notatki.ora
$ make
$ ./main

$ make build-windows
$ wine ./main.exe
```

## uwagi

- testowane na raylib 4.5. wyższe werje mogą nie działać bo coś tam
  odwalili z liczeniem wektorów lololol

``` shell
$ git clone https://github.com/raysan5/raylib
$ cd raylib
$ git checkout 4.5.0
$ cd src/
$ make
$ sudo make install
```

- wymaga raygui
- wymaga **GNU Make** (bsd make ma problem z *.SUFFIXES*)
- wzory i pomysły są wyjęte z dupy, np.:

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

## keybindingi

- *A* - dodaje sztynks
- *e* - pokazuje okno wykonywania scheme (do poprawki lolololol)

## odniesienia

- [zadanie](https://science-cup.pl/wp-content/uploads/2023/11/MSC4_2023_Optyka.pdf)
