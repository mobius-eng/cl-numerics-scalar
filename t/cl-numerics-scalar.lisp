(in-package :cl-user)
(defpackage cl-numerics-scalar-test
  (:use :cl
        :cl-numerics-scalar
        :prove))
(in-package :cl-numerics-scalar-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-numerics-scalar)' in your Lisp.

(plan nil)

(ok (num= 3d0 3.01d0 1d-2 0d0) "NUM= absolute tolerance")
(ok (num= 10d0 10.1d0 0d0 1d-2) "NUM= relative tolerance")
(ok (num= 1d0 1.000000001d0) "NUM= default tolerance")

(ok (num= 1.0
          (converge (lambda (x)
                      (declare (type single-float x))
                      (sqrt x))
                    3.0
                    30
                    1e-3
                    1e-3)
          1e-3
          1e-3)
    "CONVERGE solves x=sqrt(x)")

(ok (multiple-value-bind (x converged-p)
        (converge (lambda (x)
                    (declare (type double-float x))
                    (sqrt x))
                  3.0d0
                  10)
      (declare (ignore x))
      (not converged-p))
    "CONVERGE fails to converge x=sqrt(x) : too stringent. But doesn't hang")

(ok (num= 1d0
          (bisection (lambda (x)
                       (declare (type double-float x))
                       (- x (sqrt x)))
                     0.3d0
                     12.5d0))
    "BISECTION solves x - sqrt(x) = 0")

(ok (num= 1.0
          (false-position (lambda (x)
                            (declare (type single-float x))
                            (- x (sqrt x)))
                          0.3
                          12.5))
    "FALSE-POSITION solves x - sqrt(x) = 0")

(finalize)
