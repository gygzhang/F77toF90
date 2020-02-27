#/usr/bin/bash

python numeric2ampersand.py
./getridcvm.sh
cp -f savedir6/* ../../
cd ../..
./compile_run.sh

