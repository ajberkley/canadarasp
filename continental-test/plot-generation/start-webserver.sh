#!/bin/bash
# pkill sbcl
cd /home/ubuntu/continental-test/plot-generation
while true
do
nohup ./generate-windgram-on-demand.lisp
sleep 1
done
