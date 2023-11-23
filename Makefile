CC=clang
CFLAGS=-Wall -Wextra -I/usr/local/include
LDFLAGS=-lm -L/usr/local/lib -lraylib

CFILES=main.c
OFILES!=echo $(CFILES) | sed 's/\.c/.o/g'

.PHONY: all build-windows clean


.c.o:
	$(CC) -c $< -std=c99 $(CFLAGS)
all: $(OFILES)
	$(CC) $(OFILES) $(LDFLAGS) -o ./main
build-windows:
	[ -f "libraylib.a" ] || wget -O libraylib.a https://pub.krzysckh.org/libraylib.a
	x86_64-w64-mingw32-gcc -std=gnu11 $(CFLAGS) $(CFILES) -L. -l:libraylib.a \
		-lm -lwinmm -lgdi32 \
		-static -o main.exe
clean:
	rm -f $(TARGET) $(OFILES) *.core README.md
README.md:
	pandoc README.org -o README.md
pre-publish: clean README.md
