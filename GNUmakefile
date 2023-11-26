CC=clang
OS!=uname -s | tr -d '\n'

CSTD=-std=gnu11

ifeq "$(OS)" "OpenBSD"
LDFLAGS+=-lglfw
endif

CFLAGS=-Wall -Wextra -I. -I./src -I/usr/local/include $(CSTD) -g
LDFLAGS+=-lm -L/usr/local/lib -lraylib

SCMFILES!=echo tinyscheme/r5rs.scm `find scm -type f -name '*.scm'`

CFILES!=echo load-compiled-scripts.c \
			 `find src -type f -name '*.[cC]'` `echo $(SCMFILES) | sed 's/\.scm/.scm.c/g'`
OFILES!=echo $(CFILES) | sed 's/\.c/.o/g'

.PHONY: all build-windows clean
.SUFFIXES: .c .o .scm .scm.c

all: $(OFILES) tinyscheme/libtinyscheme.a
	$(CC) $(OFILES) ./tinyscheme/libtinyscheme.a $(LDFLAGS) -o ./main
load-compiled-scripts.c:
	./create-load-compiled-scripts.pl $(SCMFILES) > load-compiled-scripts.c
tinyscheme/libtinyscheme.a:
	$(MAKE) -C tinyscheme
.scm.scm.c:
	./f2c.pl $< > $@
.c.o:
	$(CC) -o $@ -c $< $(CFLAGS)
build-windows:
	[ -f "libraylib.a" ] || wget -O libraylib.a https://pub.krzysckh.org/libraylib.a
	[ -f "libtinyscheme-w64.a" ] || wget -O libtinyscheme-w64.a https://pub.krzysckh.org/libtinyscheme-w64.a
	$(MAKE) clean $(OFILES) CC=x86_64-w64-mingw32-gcc
	x86_64-w64-mingw32-gcc $(CFLAGS) $(OFILES) -L. -l:libraylib.a \
		-l:libtinyscheme-w64.a \
		-lm -lwinmm -lgdi32 \
		-static -o rl-optyka-test.exe
clean:
	rm -f $(TARGET) $(OFILES) *.core README.md main *.scm.c load-compiled-scripts.c *.exe
README.md:
	pandoc README.org -o README.md
pre-publish: clean README.md
