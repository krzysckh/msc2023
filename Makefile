CC=clang-16
CFLAGS=-Wall -Wextra
LDFLAGS=-lraylib -lm

CFILES=main.c
OFILES!=echo $(CFILES) | sed 's/\.c/.o/g'

.PHONY: all build-windows clean


.c.o:
	$(CC) $(CFLAGS) -c $< -std=c99
all: $(OFILES)
	$(CC) $(LDFLAGS) $(OFILES) -o ./main
build-windows:
	[ -f "libraylib.a" ] || wget -O libraylib.a https://pub.krzysckh.org/libraylib.a
	x86_64-w64-mingw32-gcc -std=gnu11 $(CFLAGS) $(CFILES) ./libraylib.a \
		$(LDFLAGS) -lm -lopengl32 -lgdi32 -lwinmm -lmingw32 -lssp \
		-fstack-protector -o main.exe
clean:
	rm -f $(TARGET) $(OFILES) *.core
