(in-package :cl-user)

;; * CL-NUMERICS-SCALAR
;; ** Package defintion and declarations
(defpackage cl-numerics-scalar
  (:use :cl)
  (:export
   #:+double-float-default-precision+
   #:+double-float-e+
   #:+double-float-pi+
   #:+single-float-default-precision+
   #:+single-float-e+
   #:+single-float-pi+
   #:+single-float-i+
   #:+double-float-i+
   #:plus-infinite-p
   #:minus-infinite-p
   #:+double-float-plus-infinity+
   #:+double-float-minus-infinity+
   #:+single-float-plus-infinity+
   #:+single-float-negative-infinity+
   #:num=
   #:converge
   #:*converge-max-iterations*
   #:bisection
   #:false-position))

(in-package :cl-numerics-scalar)

(declaim (optimize (speed 3) (debug 1) (safety 1)))


;; ** Floating point constants
;; *** Default precision
(defconstant +double-float-default-precision+
  #.(sqrt double-float-epsilon)
  "Constant that should be considered as a default precision for
   DOUBLE-FLOAT computations: (SQRT EPSILON)")

(defconstant +single-float-default-precision+
  #.(sqrt single-float-epsilon)
  "Constant that should be considered as a default precision for
   SINGLE-FLOAT computations: (SQRT EPSILON)")

;; *** Euler constant e = 2.718281828...
(defconstant +single-float-e+ #.(exp 1.0)
  "SINGLE-FLOAT Euler constant e = 2.71828...")

(defconstant +double-float-e+ #.(exp 1d0)
  "DOUBLE-FLOAT Euler constant e = 2.71828...")

;; *** Pi = 3.14...
(defconstant +double-float-pi+ #.(coerce pi 'double-float)
  "DOUBLE-FLOAT pi number")

(defconstant +single-float-pi+ #.(coerce pi 'single-float)
  "SINGLE-FLOAT pi number")

;; *** Imaginary one i
(defconstant +single-float-i+ #C(0.0 1.0)
  "SINGLE-FLOAT imaginary one")

(defconstant +double-float-i+ #C(0d0 1d0)
  "DOUBLE-FLOAT imaginary one")

;; *** Infinties
;; Infinities are not so useful in CL as division by zero
;; usually produces an error. However, they are useful to
;; indicate, e.g. infinite boundary of integration
;; (see CL-NUMERICS QUAD.LISP)
(defgeneric plus-infinite-p (number))
(defgeneric minus-infinite-p (number))

(defmethod plus-infinite-p ((number number)) nil)
(defmethod minus-infinite-p ((number number)) nil)

#-clisp
(defmethod plus-infinite-p ((number number))
  (> number most-positive-double-float))

#-clisp
(defmethod minus-infinite-p ((number double-float))
  (< number most-negative-double-float))

#-clisp
(defmethod plus-infinite-p ((number single-float))
  (> number most-positive-single-float))

#-clisp
(defmethod minus-infinite-p ((number single-float))
  (< number most-negative-single-float))

#-clisp
(defconstant +double-float-plus-infinity+
  #+sbcl sb-ext:double-float-positive-infinity
  #+ccl 1d++0
  #+ecl ext:double-float-positive-infinity)

#-clisp
(defconstant +double-float-minus-infinity+
  #+sbcl sb-ext:double-float-negative-infinity
  #+ccl -1d++0
  #+ecl ext:double-float-negative-infinity)

#-clisp
(defconstant +single-float-plus-infinity+
  #+sbcl sb-ext:single-float-positive-infinity
  #+ccl 1e++0
  #+ecl ext:single-float-positive-infinity)

#-clisp
(defconstant +single-float-negative-infinity+
  #+sbcl sb-ext:single-float-negative-infinity
  #+ccl -1e++0
  #+ecl ext:single-float-negative-infinity)


;; ** Utils on CL types: extract parts and flatten
;; *** TYPE-HEAD
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline type-head))
  (defun type-head (type-spec)
    "Take the main type identifier"
    (declare (optimize (speed 3) (debug 1) (safety 1)))
    (if (consp type-spec)
        (car type-spec)
        type-spec)))

;; *** DECOMPOSE-TYPE, SIMPLIFY-TYPE-NAME, MAKE-DEFAULT-TOLERANCE
;; These functions are used in macros, thus inside =EVAL-WHEN= form.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun decompose-type (type-spec)
    "Decomposes type to its two basic components.
  For example: (COMPLEX (SINGLE-FLOAT 2.0 3.0)) => (VALUES COMPLEX SINGLE-FLOAT)
  and DOUBLE-FLOAT => DOUBLE-FLOAT"
    (if (consp type-spec)
        (values (car type-spec) (type-head (cadr type-spec)))
        (values type-spec nil)))

  (defun simplify-type-name (type)
    (if (consp type) 
        (format nil "~{~A~^-~}" type)
        type))

  (defun make-default-tolerance (type)
    (multiple-value-bind (main-type subtype) (decompose-type type)
      (values
       (if (null subtype)
           (intern (format nil "+~A-DEFAULT-PRECISION+" main-type))
           (intern (format nil "+~A-DEFAULT-PRECISION+" subtype)))
       (if (null subtype) main-type subtype)))))

;; ** Approximate comparison NUM=
;; *** NUM= - main function
(defun num= (x y &optional (abs-tolerance nil atol-p) (rel-tolerance nil rtol-p))
  "Test equality between X and Y within the tolerance given by
  ABS-TOLERANCE and REL-TOLERANCE.
  The test is
  |x - y| < a + r max(|x|,|y|)
  with a = ABS-TOLERANCE and r = REL-TOLERANCE"
  (multiple-value-bind (type subtype) (decompose-type (type-of x))
    (let ((arglist `(,type
                     ,subtype
                     ,x ,y
                     ,@(if atol-p (list abs-tolerance) nil)
                     ,@(if rtol-p (list rel-tolerance) nil))))
      (apply #'num=-dispatch arglist))))

;; *** ~NUM=-dispatch~: dispatching on type
(defgeneric num=-dispatch (type subtype x y &optional atol rtol)
  (:documentation
   "Dispatch function for NUM=.
   TYPE is the main type id of argument X
   SUBTYPE is either NIL or element type for COMPLEX"))

;; *** MAKE-NUM= : macro making NUM= for different types
(defmacro make-num= (type)
  "Makes types-specialized NUM="
  (let* ((simplified-type-name (simplify-type-name type))
         (function-name (intern (format nil "~A-NUM=" simplified-type-name)))
         (documentation "Specialized version of NUM="))
    (multiple-value-bind (tol-default tol-type) (make-default-tolerance type)
      (multiple-value-bind (main-type subtype) (decompose-type type)
        `(progn
           (defun ,function-name (x y &optional
                                        (abs-tolerance ,tol-default)
                                        (rel-tolerance ,tol-default))
             ,documentation
             (declare (optimize (speed 3) (debug 1) (safety 1))
                      (type ,type x y)
                      (type ,tol-type abs-tolerance rel-tolerance))
             (let ((f (max (abs x) (abs y))))
               (declare (type ,tol-type f))
               (< (abs (- x y)) (+ abs-tolerance (* rel-tolerance f)))))
           (defmethod num=-dispatch
               ((mt (eql ',main-type)) (st (eql ',subtype))
                x y
                &optional (atol ,tol-default) (rtol ,tol-default))
             (,function-name x y atol rtol))
           (export ',function-name))))))

;; *** Generate NUM= for different types
(make-num= double-float)
(make-num= single-float)
(make-num= (complex double-float))
(make-num= (complex single-float))

;; ** CONVERGE: solves x=f(x)
;; *** CONVERGE-DISPATCH
(defgeneric converge-dispatch (type subtype function init-value
                               &optional
                                 max-iterations
                                 abs-tolerance
                                 rel-tolerance)
  (:documentation "Simple iteration algorithm solving x=f(x)"))

;; *** *CONVERGE-MAX-ITERATIONS*
(defvar *converge-max-iterations* 100)

;; *** CONVERGE
(defun converge (function init-value
                 &optional
                   (max-iterations *converge-max-iterations*)
                   (abs-tolerance nil atol-p) (rel-tolerance nil rtol-p))
  (multiple-value-bind (type subtype) (decompose-type (type-of init-value))
    (let ((arglist `(,type
                     ,subtype
                     ,function
                     ,init-value
                     ,max-iterations
                     ,@(if atol-p (list abs-tolerance) nil)
                     ,@(if rtol-p (list rel-tolerance) nil))))
      (apply #'converge-dispatch arglist))))

;; *** MAKE-CONVERGE: macro generating CONVERGE-*
(defmacro make-converge (type)
  (let* ((simplified-type-name (simplify-type-name type))
         (function-name (intern (format nil "CONVERGE-~A" simplified-type-name)))
         (documentation "Specialized version of CONVERGE")
         (num=-function (intern (format nil "~A-NUM=" simplified-type-name))))
    (multiple-value-bind (main-type subtype) (decompose-type type)
      (multiple-value-bind (def-tol tol-type) (make-default-tolerance type)
        `(progn
           (defun ,function-name (function init-value
                                  &optional
                                    (max-iterations *converge-max-iterations*)
                                    (abs-tolerance ,def-tol)
                                    (rel-tolerance ,def-tol))
             ,documentation
             (declare (type function function)
                      (type ,type init-value)
                      (type ,tol-type abs-tolerance rel-tolerance)
                      (type fixnum max-iterations))
             (let ((result init-value))
               (declare (type ,type result))
               (dotimes (i max-iterations (values result nil max-iterations))
                 (setf init-value result)
                 (setf result (funcall function init-value))
                 (when (,num=-function result init-value abs-tolerance rel-tolerance)
                   (return (values result t i))))))
           (defmethod converge-dispatch
               ((tp (eql ',main-type)) (st (eql ',subtype)) function init-value
                &optional
                  (max-iterations *converge-max-iterations*)
                  (abs-tolerance ,def-tol)
                  (rel-tolerance ,def-tol))
             (,function-name function init-value max-iterations abs-tolerance rel-tolerance))
           (export ',function-name))))))

;; *** Generate CONVERGE for different types
(make-converge double-float)
(make-converge single-float)
(make-converge (complex double-float))
(make-converge (complex single-float))

;; ** BISECTION: solve f(x)=0
;; *** Generic BISECTION
(defgeneric bisection (function left right &optional abs-tolerance rel-tolerance)
  (:documentation "Bisection method of solving f(x)=0 on interval (left right).
  Only works on reals"))

;; *** MAKE-BISECTION: macro generating BISECTION
(defmacro make-bisection (type)
  (let* ((simplified-type-name (simplify-type-name type))
         (function-name (intern (format nil "BISECTION-~A" simplified-type-name)))
         (documentation "Specialized version of BISECTION")
         (num=-function (intern (format nil "~A-NUM=" simplified-type-name)))
         (def-tol (make-default-tolerance type)))
    `(progn
       (defun ,function-name (function left right
                              &optional
                                (abs-tolerance ,def-tol)
                                (rel-tolerance ,def-tol))
         ,documentation
         (declare (type function function)
                  (type ,type left right abs-tolerance rel-tolerance))
         (do* ((centre (/ (+ left right) 2) (/ (+ left right) 2))
               (fleft (funcall function left))
               (fright (funcall function right))
               (fcentre (funcall function centre) (funcall function centre)))
              ((,num=-function left right abs-tolerance rel-tolerance) centre)
           (declare (type ,type centre fleft fright fcentre))
           (unless (plusp (* fleft fcentre))
             (setf right centre fright fcentre))
           (unless (plusp (* fcentre fright))
             (setf left centre fleft fcentre))))
       (defmethod bisection (function (left ,type) (right ,type)
                             &optional
                               (abs-tolerance ,def-tol)
                               (rel-tolerance ,def-tol))
         (,function-name function left right abs-tolerance rel-tolerance))
       (export ',function-name))))

;; *** Generate BISECTION for different types
(make-bisection double-float)
(make-bisection single-float)

;; ** FALSE-POSITION
;; *** Generic FALSE-POSITION
(defgeneric false-position (function left right
                            &optional abs-tolerance rel-tolerance)
  (:documentation "False position solver f(x)=0 on the interval (left right)"))

;; *** MAKE-FALSE-POSITION: macro
(defmacro make-false-position (type)
  (let* ((simplified-type-name (simplify-type-name type))
         (function-name (intern (format nil "FALSE-POSITION-~A" simplified-type-name)))
         (documentation "Specialized version of FALSE-POSITION")
         (num=-function (intern (format nil "~A-NUM=" simplified-type-name)))
         (def-tol (make-default-tolerance type)))
    `(progn
       (defun ,function-name (function left right
                              &optional
                                (abs-tolerance ,def-tol)
                                (rel-tolerance ,def-tol))
         ,documentation
         (declare (type function function)
                  (type ,type left right abs-tolerance rel-tolerance))
         (do* ((fleft (funcall function left))
               (fright (funcall function right))
               (kept-left-before nil keep-left)
               (keep-left nil)
               (centre #1=(- left (/ (* fleft (- right left))
                                     (- fright fleft)))
                       #1#)
               (fcentre #2=(funcall function centre) #2#))
              ((,num=-function left right abs-tolerance rel-tolerance) centre)
           (declare (type ,type fleft fright centre fcentre))
           (unless (plusp (* fcentre fleft))
             (setf right centre fright fcentre keep-left t))
           (unless (plusp (* fcentre fright))
             (setf left centre fleft fcentre keep-left nil))
           (when (= left right) (return left))
           (when (and keep-left kept-left-before)
             (setf fleft (/ fleft 2)))
           (when (and (not keep-left) (not kept-left-before))
             (setf fright (/ fright 2)))))
       (defmethod false-position
           (function (left ,type) (right ,type)
            &optional
              (abs-tolerance ,def-tol)
              (rel-tolerance ,def-tol))
         (,function-name function left right abs-tolerance rel-tolerance))
       (export ',function-name))))

;; *** Generate FALSE-POSITION for differnt types
(make-false-position double-float)
(make-false-position single-float)




;; (defgeneric horner-rule (polynomial number)
;;   (:documentation "Horner's rule of polynomial evaluation"))

;; (defmethod horner-rule ((polynomial cons) number)
;;   "Polynomial coefficients must be stored from heighest to lowerest powers"
;;   (let ((result (first polynomial)))
;;     (dolist (coeff (rest polynomial) result)
;;       (setf result (+ coeff (* number result))))))

;; (defmethod horner-rule ((polynomial vector) number)
;;   "Polynomial coefficients must be stored from lowest to heighest powers"
;;   (let* ((length (length polynomial))
;;          (result (aref polynomial (1- length))))
;;     (loop for i from (- length 2) downto 0
;;        do (setf result (+ (aref polynomial i) (* number result)))
;;        finally (return result))))

;; * END
