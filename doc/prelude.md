# co, gdzie i jak

Główna część programu napisana jest w C, jednakże by ułatwić i usprawnić proces
tworzenia, wbudowałem do niej [interpreter scheme](https://tinyscheme.sourceforge.net/).

Każdy nowy plik w folderze `scm/` z rozszerzeniem `.scm` będzie linkowany
z programem i uruchamiany na samym początku. Te pliki służą do definiowania
"systemowych" funkcji, a więc (w przyszłości) UI, wszystkiego związanego
z przemieszczaniem obiektów na ekranie, etc.

Rdzeń myślący i obliczający rzeczy zostanie jednak w C.

# dokumentacja pre-definiowanych funkcji dla scheme:
