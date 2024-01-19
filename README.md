# quickstart

## unix

``` shell
$ cd /tmp
$ git clone https://github.com/raysan5/raylib
$ cd raylib
$ git checkout 4.5.0
$ cd src/
$ make
$ sudo make install

$ cd /miejsce/na/folder-z-kodem
$ git clone https://git.krzysckh.org/kpm/science-cup-2023
$ cd science-cup-2023
$ make
$ ./main
```

## windows

- lol
- [rl-optyka-test.exe](https://pub.krzysckh.org/rl-optyka-test.exe)
- [rc.scm](https://pub.krzysckh.org/rc.scm)

# dokumentacja

- [zdefiniowane dla tinyscheme](https://pub.krzysckh.org/msc2023.html)
- TBD

## uwagi
- [koloryt](https://user-images.githubusercontent.com/58662350/214382274-0108806d-b605-4047-af4b-c49ae06a2e8e.png)
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

## odniesienia

- [zadanie](https://science-cup.pl/wp-content/uploads/2023/11/MSC4_2023_Optyka.pdf)
