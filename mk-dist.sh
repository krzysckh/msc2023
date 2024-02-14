#!/bin/sh

# shellcheck nie płacz, wszystko będzie dobrze
# ~ kpm

set -xe

make="$1"
cur=$(pwd)

pdoc() {
  case "$2" in
    *pdf)  addarg="--pdf-engine=xelatex" ;;
    *html) addarg="-H $cur/doc/doc.css"  ;;
  esac

  pandoc "$1" --toc --toc-depth=2 \
         $addarg \
         --metadata title="$(basename $1 | cut -f1 -d.)" -f gfm \
         --standalone -o "$2"
}

rm -rf /tmp/msc2023*
rm -f /tmp/lambda-optyka.exe

$make build-windows

cp ./rl-optyka-test.exe /tmp/lambda-optyka.exe
$make full-clean

cd /tmp/ || exit 1

cp -r $cur msc2023
tar --exclude-vcs --exclude-vcs-ignores -cvzf msc2023-src.tgz msc2023
mkdir msc2023-dist

cp msc2023-src.tgz ./msc2023-dist/
cp /tmp/lambda-optyka.exe ./msc2023-dist/
cp $cur/doc/USER-MANUAL.md ./msc2023-dist/README.md
pdoc ./msc2023-dist/README.md ./msc2023-dist/README.html
pdoc ./msc2023-dist/README.md ./msc2023-dist/README.pdf

tar cvzf msc2023-dist.tgz msc2023-dist
cp msc2023-dist.tgz $cur/
