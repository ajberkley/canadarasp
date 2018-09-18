This package provides minimal set of [CFFI](http://common-lisp.net/project/cffi/)-based Common Lisp bindings to the
[GDAL/OGR library](http://www.gdal.org/).

It is still a work in progress and not ready for prime-time use.

All development happens in the `devel` branch; `default` tends to be more stable.

This library will offer two levels of access to the native library:

* close to the raw C API with manual garbage collecion; and
* CLOS-based object-oriented wrapper with automatic garbage collection (based on [`trivial-garbage`](http://common-lisp.net/project/trivial-garbage/)).

Also you might be interested in the [`cl-proj`](http://cl-proj.sourceforge.net/): a Common Lisp wrapper for the Proj.4 library.

This library is tested on SBCL, Clozure CL, and Embedded CL.