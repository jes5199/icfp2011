#!/bin/bash
set -e
echo `git rev-parse HEAD`

export PATH="$PATH:/var/lib/gems/1.8/bin/"

cd simulator
make
./simtester
cd ..

cd brain
make
./braintester
cd ..

mkdir -p test_cases
brain/genTestCases test_cases
