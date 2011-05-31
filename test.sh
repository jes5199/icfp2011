#!/bin/bash
echo "LABEL=seconds" | tee results.properties
echo "YVALUE=`date +%S`" | tee -a results.properties

