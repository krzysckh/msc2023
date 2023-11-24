CC=clang
CFLAGS=-Wall -Wextra -I/usr/local/include -g
LDFLAGS=-lm -L/usr/local/lib -lraylib

CFILES=init.scm.c main.c
OFILES!=echo $(CFILES) | sed 's/\.c/.o/g'

.PHONY: all build-windows clean

all: $(OFILES) init.scm.c tinyscheme/libtinyscheme.a
	$(CC) $(OFILES) ./tinyscheme/libtinyscheme.a $(LDFLAGS) -o ./main
tinyscheme/libtinyscheme.a:
	$(MAKE) -C tinyscheme
init.scm.c:
	./f2c.pl tinyscheme/init.scm > init.scm.c
.c.o:
	$(CC) -c $< -std=c99 $(CFLAGS)
build-windows: init.scm.c
	[ -f "libraylib.a" ] || wget -O libraylib.a https://pub.krzysckh.org/libraylib.a
	[ -f "libtinyscheme-w64.a" ] || wget -O libtinyscheme-w64.a https://pub.krzysckh.org/libtinyscheme-w64.a
	x86_64-w64-mingw32-gcc -std=gnu11 $(CFLAGS) $(CFILES) -L. -l:libraylib.a \
		-l:libtinyscheme-w64.a \
		-lm -lwinmm -lgdi32 \
		-static -o main.exe
clean:
	rm -f $(TARGET) $(OFILES) *.core README.md main init.scm.c
README.md:
	pandoc README.org -o README.md
pre-publish: clean README.md
