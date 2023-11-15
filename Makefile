.PHONY: build-win build-native run

MAIN=src/main.rkt

build-win: build
	raco cross --target x86_64-win exe --embed-dlls --gui -o ./build/main-win.exe ${MAIN}
build-native: build
	raco exe --gui -o ./build/main-native ${MAIN}
build:
	mkdir -p ./build
run:
	racket ${MAIN}
