#!/bin/sh
while true
do
    echo Starting server
    sbcl --script timezone.lisp
    sleep 5
done
