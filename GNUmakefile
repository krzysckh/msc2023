CC=clang
OS!=uname -s | tr -d '\n'
TARGET=./main

CSTD=-std=gnu11

CFLAGS=-Wall -Wextra -I. -I./src -I/usr/local/include $(CSTD) -g $(ACFLAGS)
LDFLAGS=-lm -L/usr/local/lib -lraylib $(ALDFLAGS)

SCMFILES!=echo tinyscheme/r5rs.scm `find scm -type f -name '*.scm'`

CFILES!=echo load-compiled-scripts.c \
	proggy.c \
	`find src -type f -name '*.[cC]'` `echo $(SCMFILES) | sed 's/\.scm/.scm.c/g'`
OFILES!=echo $(CFILES) | sed 's/\.c/.o/g'

ifeq "$(OS)" "OpenBSD"
LDFLAGS:=-lglfw $(LDFLAGS)
endif

.PHONY: all build-windows clean doc
.SUFFIXES: .c .o .scm .scm.c .otf .c

all: $(OFILES) tinyscheme/libtinyscheme.a
	$(CC) $(OFILES) ./tinyscheme/libtinyscheme.a $(LDFLAGS) -o $(TARGET)
load-compiled-scripts.c:
	./create-load-compiled-scripts.pl $(SCMFILES) > load-compiled-scripts.c
tinyscheme/libtinyscheme.a:
	$(MAKE) -C tinyscheme
.otf.c:
	xxd --include $< > $@
.scm.scm.c:
	./f2c.pl $< > $@
.c.o:
	$(CC) -o $@ -c $< $(CFLAGS)
build-windows:
	[ -f "libraylib.a" ] || wget -O libraylib.a https://pub.krzysckh.org/libraylib.a
	[ -f "libtinyscheme-w64.a" ] || wget -O libtinyscheme-w64.a https://pub.krzysckh.org/libtinyscheme-w64.a
	$(MAKE) clean $(OFILES) CC=x86_64-w64-mingw32-gcc ACFLAGS=-O2
	x86_64-w64-mingw32-gcc -g -O2 $(CFLAGS) $(OFILES) -L. -l:libraylib.a \
		-l:libtinyscheme-w64.a \
		-lm -lwinmm -lgdi32 \
		-static -o rl-optyka-test.exe
clean:
	rm -f $(TARGET) $(OFILES) *.core main *.scm.c load-compiled-scripts.c *.exe scdoc.html
	rm -f ./doc/msc2023.html ./doc/scheme.md
full-clean:
	$(MAKE) -C tinyscheme clean
	$(MAKE) clean
doc: all
	$(TARGET) -F ./generate-docs.scm > doc/scheme.md
	cat doc/prelude.md doc/scheme.md \
		| pandoc --toc --toc-depth=2 \
                --metadata title="msc2023" -f gfm -t html \
		--standalone -o doc/msc2023.html
pubcpy:
	([ `whoami` = "krzych" ] || [ `whoami` = "kpm" ]) || exit 1

	$(MAKE) clean doc
	cd doc && yes | pubcpy ./msc2023.html
	$(MAKE) clean build-windows
	yes | pubcpy ./rl-optyka-test.exe
	yes | pubcpy ./rc.scm
	$(MAKE) clean
cloc: clean
	cloc `ls | grep -v tinyscheme`
