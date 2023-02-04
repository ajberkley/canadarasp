#!/bin/sh
mkdir -p ~/.config/common-lisp/source-registry.conf.d
cd /home/ubuntu
git clone https://github.com/ajberkley/cl-png.git
git clone https://github.com/ajberkley/cl-gdal.git
git clone https://github.com/ajberkley/mmap-shared-cache.git
echo '(:tree "/home/ubuntu/cl-png")' > ~/.config/common-lisp/source-registry.conf.d/10-cl-png.conf
echo '(:tree "/home/ubuntu/cl-gdal")' > ~/.config/common-lisp/source-registry.conf.d/11-cl-gdal.conf
echo '(:tree "/home/ubuntu/mmap-shared-cache")' > ~/.config/common-lisp/source-registry.conf.d/12-mmap-shared-cache.conf
sbcl --eval '(progn (quicklisp:quickload "png") (quicklisp:quickload "osicat") (quicklisp:quickload "alexandria") (quicklisp:quickload "cl-gd") (quicklisp:quickload "cl-ppcre") (quit))'
