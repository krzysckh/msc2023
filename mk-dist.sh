#!/bin/sh

# shellcheck nie płacz, wszystko będzie dobrze
# ~ kpm

set -xe

make="$1"
cur=$(pwd)

rm -rf /tmp/msc2023*
rm -f /tmp/lambda-optyka.exe

cp ./doc/USER-MANUAL.pdf  /tmp/README.pdf

git archive --format=tgz --prefix=msc2023-src/ HEAD > /tmp/msc2023-src.tgz

$make doc
cp ./doc/msc2023.pdf /tmp/msc2023-prog-man.pdf

$make build-windows
cp ./rl-optyka-test.exe /tmp/lambda-optyka.exe

$make full-clean

cd /tmp/ || exit 1

cp -r $cur msc2023
mkdir msc2023-dist

cp msc2023-src.tgz ./msc2023-dist/
cp /tmp/lambda-optyka.exe ./msc2023-dist/
cp /tmp/msc2023-prog-man.pdf ./msc2023-dist/

cp README.pdf  ./msc2023-dist/

tar cvzf msc2023-dist.tgz msc2023-dist
zip -r msc2023-dist.zip msc2023-dist
cp msc2023-dist.tgz $cur/
cp msc2023-dist.zip $cur/
