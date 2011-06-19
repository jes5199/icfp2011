#!/bin/bash
cd simulator
make
cd ..

bin/ltg match ./simulator/braindriver ./simulator/braindriver 2>&1 | tee match_results
