# Uruchamianie

- windows: załączony został plik lambda-optyka.exe - wystarczy go uruchomić
- GNU/Linux: plik skompilowany na debianie 12 można pobrać [tutaj](https://pub.krzysckh.org/msc2023-lambda-optyka-linux-x86_64), lub skompilować ze źródeł (patrz unix)
- unix:

```sh
$ tar xvzf msc2023-src.tgz
$ cd msc2023-src
$ grep '^\$[^\$]' README.md
```

(można też użyć [zkonteneryzowanego procesu kompilacji](https://git.krzysckh.org/kpm/msc2023-docker))

# Podstawowe używanie

### GUI

- Na górze wyświetlają się informacje od programu - błędy, podpowiedzi itp.,
- Na dole po lewej, wyświetla się aktualny tryb programu, domyślny tryb to `normal-mode` - tryb biernego
  oglądania symulacji. W trakcie korzystania z przeróżnych menu tryb się zmienia.
  Nie jest to ważna informacja, ale może się przydać, jeśli nie wiemy co aktualnie się dzieje ;).

### Przyciski myszy
- Prawym przyciskiem myszy można otworzyć menu programu - służy ono do wszystkiego,
  w tym **wstawiania nowych elementów**. Jeśli kliknięto nim na już stworzony obiekt, wyświetli się menu
  opcji dla klikniętego elementu.
- Przytrzymując lewy przycisk myszy można zaznaczać elementy. Po zaznaczeniu można je kopiować bądź usuwać.
  jeśli wszystkie zaznaczone elementy są tego samego typu, można masowo zmieniać opcje wszystkim na raz.

### Podstawowe klawisze
- klawisz <kbd> shift </kbd> + <kbd> a </kbd> to skrót do tworzenia nowego, domyślnego źródła.
- klawisz <kbd> q </kbd> to skrót do wyjścia z programu.

### Dodatkowe klawisze
- klawisz <kbd> shift </kbd> + <kbd> m </kbd> wyłączy wyświetlanie dolnego napisu o aktualnym trybie programu. 
  Ponowne wciśnięnie z powrotem włączy ten napis.
- klawisz <kbd> shift </kbd> + <kbd> r </kbd> włączy możliwość zwiększania/zmniejszania okna.
  Ponowne wciśnięcie wyłączy tę możliwość.
- klawisz <kbd> ` </kbd> pokaże informacje o ilości aktualnie używanych hooków i FPS.
- klawisz <kbd> ~ </kbd> pokaże menu pozwalające na zmienianie domyślnych ustawień okna.
- klawisz <kbd> e </kbd> to skrót do okna do ewaluowania wyrażeń scheme.

# Przykłady

Załączone wbudowane przykłady dostępne są w programie - dostać można się do nich z menu głównego.

# Więcej informacji

Dodatkowe informacje o bardziej zaawansowanych użyciach programu załączone zostały w pliku `msc2023-prog-man.pdf` (warto przeczytać - przynajmniej wstęp).
Zachęcam użytkownika do zabawy z wbudowanym językiem, ponieważ pomaga on w tworzeniu ciekawych symulacji (np.: [animowanych](https://git.krzysckh.org/kpm/science-cup-2023/src/commit/c01b3968497c8b44b6dab7da1110cec04716ac17/scm/e.scm#L38-L64)).

Testowane na:
- [Debian GNU/Linux](https://www.debian.org/) (kpm),
- [OpenBSD](https://www.openbsd.org/74.html) (kpm),
- MS Windows 7 (kpm),
- MS Windows 10 (kpm),
- [9front](http://9front.org) ([kpm](https://git.krzysckh.org/kpm/msc9))

# Zastrzeżenia

- program ma bardzo szorstkie krawędzie,
- program gryzie po kostkach,
- w internecie nikt nie wie, że jesteś psem

# Podziękowania

Dziękuję:
- Fyrdan Dyrdan - za pomoc psychiczną,
- Jezus Chrystus - za zbawienie,
- Mojej drużynie - za - *sumaryczne* - napisanie 0 linijek kodu.
