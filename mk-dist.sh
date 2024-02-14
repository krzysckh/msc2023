#!/bin/sh

set -xe

make="$1"
cur=$(pwd)

rm -rf /tmp/msc2023*
rm -f /tmp/lambda-optyka.exe

$make clean
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

tar cvzf msc2023-dist.tgz msc2023-dist
cp msc2023-dist.tgz $cur/
