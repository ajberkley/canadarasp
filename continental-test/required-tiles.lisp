#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(quicklisp:quickload "cl-fad" :silent t)
(quicklisp:quickload "cl-ppcre" :silent t)
(use-package :cl-fad)
(use-package :cl-ppcre)
(load "model-parameters.lisp")
(load "required-tile-iterator.lisp")

(defun output-required-tiles ()
  "Output a file with the windgram tile directories one per line."
  (let ((iterator (only-required-tile-iterator)))
    (loop
       for tile-id = (car (funcall iterator)) ;; grab the directory-name
       while tile-id
       do (format t "~&~A/~A/" *tiledir* tile-id))))

(output-required-tiles)
