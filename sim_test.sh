#!/bin/bash
set -e
echo `git rev-parse HEAD`

bin/run_test_cases `ls test_cases`
