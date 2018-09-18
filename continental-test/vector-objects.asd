;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:vector-objects-asd
  (:use :cl :asdf :iterate))

(in-package :vector-objects-asd)

(defsystem vector-objects
  :name "vector-objects"
  :version "0.0.0"
  :maintainer "Andrew J. Berkley"
  :author "Andrew J. Berkley"
  :licence "BSD 3 Clause"
  :description "Minimal draw and manipulate vector objects"
  :components ((:file "vector-objects")))
