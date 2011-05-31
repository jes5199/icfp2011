#!/bin/bash
echo "label=seconds" | tee results.properties
echo "y=`date +%S`" | tee -a results.properties

