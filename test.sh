#!/bin/bash
echo "time" | tee results.csv
date +%s | tee -a results.csv

