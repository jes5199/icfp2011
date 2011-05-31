#!/bin/bash
echo "time,seconds" | tee results.csv
echo "`date +%s`,`date +%S`" | tee -a results.csv

