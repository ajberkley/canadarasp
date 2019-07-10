#!/bin/sh
while true
do
    echo Starting server
    sbcl --script timezone.lisp
done
