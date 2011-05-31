#!/bin/bash
echo "time=`date +%s`" | tee results.properties
echo "seconds=`date +%S`" | tee -a results.properties

