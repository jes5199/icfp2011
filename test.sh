#!/bin/bash
set -e
echo `git rev-parse HEAD`

export PATH="$PATH:/var/lib/gems/1.8/bin/"

cd simulator
make
./simtester
cd ..

cd driver
make
cd ..
