#!/bin/sh
while true
do
    echo Starting server
    nohup sbcl --load "/home/ubuntu/quicklisp/setup.lisp" --script timezone.lisp &
done
