CC=clang
CFLAGS=-Wall -Wextra `pkg-config --cflags raylib | tr -d '"'`
LDFLAGS=-lm `pkg-config --libs raylib | tr -d '"'`

CFILES=main.c
OFILES!=echo $(CFILES) | sed 's/\.c/.o/g'

.PHONY: all build-windows clean


.c.o:
	$(CC) -c $< -std=c99 $(CFLAGS)
all: $(OFILES)
	$(CC) $(LDFLAGS) $(OFILES) -o ./main
build-windows:
	[ -f "libraylib.a" ] || wget -O libraylib.a https://pub.krzysckh.org/libraylib.a
	x86_64-w64-mingw32-gcc -std=gnu11 $(CFLAGS) $(CFILES) -L. -l:libraylib.a \
		-lm -lwinmm -lgdi32 \
		-static -o main.exe
clean:
	rm -f $(TARGET) $(OFILES) *.core README.md
README.md:
	pandoc README.org -o README.md
