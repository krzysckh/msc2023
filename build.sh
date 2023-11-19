#!/bin/sh

CC=clang-16
CFLAGS="-Wall -Wextra -std=c99"
LDFLAGS="-lraylib -lm"

rm ./main
$CC $CFLAGS $LDFLAGS main.c -o ./main

if [ $BUILD_WINDOWS = "yes" ]; then
  [ -f "libraylib.a" ] || wget -O libraylib.a https://pub.krzysckh.org/libraylib.a
  x86_64-w64-mingw32-gcc $CFLAGS ./main.c ./libraylib.a \
                         -lm -lopengl32 -lgdi32 -lwinmm -lmingw32 \
                         -lssp -fstack-protector \
                         -o main.exe
fi
