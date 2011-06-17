#!/bin/bash
set -e
echo `git rev-parse HEAD`
cd simulator
make
./simtester
cd ..
