CC=clang
CFLAGS=-Wall -Wextra -I. -I./src -I/usr/local/include -g
LDFLAGS=-lm -L/usr/local/lib -lraylib

SCMFILES=tinyscheme/r5rs.scm

CFILES=`find src -type f -name '*.[cC]'` `echo $(SCMFILES) | sed 's/\.scm/.scm.c/g'`
OFILES!=echo $(CFILES) | sed 's/\.c/.o/g'

.PHONY: all build-windows clean
.SUFFIXES: .c .o .scm .scm.c

all: $(OFILES) tinyscheme/libtinyscheme.a
	$(CC) $(OFILES) ./tinyscheme/libtinyscheme.a $(LDFLAGS) -o ./main
tinyscheme/libtinyscheme.a:
	$(MAKE) -C tinyscheme
.scm.scm.c:
	./f2c.pl $< > $@
.c.o:
	$(CC) -o $@ -c $< $(CFLAGS) -std=c99
build-windows:
	[ -f "libraylib.a" ] || wget -O libraylib.a https://pub.krzysckh.org/libraylib.a
	[ -f "libtinyscheme-w64.a" ] || wget -O libtinyscheme-w64.a https://pub.krzysckh.org/libtinyscheme-w64.a
	x86_64-w64-mingw32-gcc -std=gnu11 $(CFLAGS) $(CFILES) -L. -l:libraylib.a \
		-l:libtinyscheme-w64.a \
		-lm -lwinmm -lgdi32 \
		-static -o main.exe
clean:
	rm -f $(TARGET) $(OFILES) *.core README.md main *.scm.c
README.md:
	pandoc README.org -o README.md
pre-publish: clean README.md
