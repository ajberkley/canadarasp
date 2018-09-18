#!/bin/bash
pkill sbcl
cd /home/ubuntu/continental-test/plot-generation
nohup ./generate-windgram-on-demand.lisp &
