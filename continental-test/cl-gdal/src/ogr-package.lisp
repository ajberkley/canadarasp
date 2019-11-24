;;; -*- package: CL-USER; Syntax: Common-lisp; Base: 10 -*-

(in-package :cl-user)

(cffi:define-foreign-library libgdal
    (:unix (:or "libgdal.so.20" "libgdal.so"))
  (t (:default "libgdal")))

(cffi:use-foreign-library libgdal)

;; --------------------------------------------------------

(defpackage :cl-ogr
  (:use :cl)
  (:nicknames :ogr)
  (:export #:data-source))

;; EOF
