#!/bin/sh
while true
do
    echo Starting server
    nohup sbcl --load "/home/ubuntu/quicklisp/setup.lisp" --script /home/ubuntu/canadarasp/web-server/timezone/timezone.lisp
    sleep 1
done
