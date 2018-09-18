#!/bin/sh
# --script stops --dynamic-space-size from working :(
# takes 32 seconds
/usr/local/bin/sbcl --dynamic-space-size 6000 --load "generate-new-variables.lisp" --eval "(quit)" $@
