``` shell
$ mypaint ./notatki.ora
$ make
$ ./main

$ make build-windows
$ wine ./main.exe
```

### uwagi

-   nie edytuj README.md - jest generowane automatycznie z README.org
-   testowane na raylib 4.5. wyższe werje mogą nie działać bo coś tam
    odwalili z liczeniem wektorów lololol
-   wzory i pomysły są wyjęte z dupy, np

$$ H = \text{wysokość okna} $$

$$
\operatorname{target}(a, b, k) = \begin{cases}
\begin{bmatrix}
  \frac{x_{a} + y_{a}(x_{b} - x_{a})}{y_{a} - y_{b}} \\
  0
\end{bmatrix} & \text{ jeśli } -180 \le k \le 0
\\
\begin{bmatrix}
  \frac{x_{a} + (x_{b} - x_{a})(H - y_{a})}{y_{b} - y_{a}} \\
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

[zadanie](https://science-cup.pl/wp-content/uploads/2023/11/MSC4_2023_Optyka.pdf)
