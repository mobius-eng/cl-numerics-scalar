#|
  This file is a part of cl-numerics-scalar project.
|#

(in-package :cl-user)
(defpackage cl-numerics-scalar-asd
  (:use :cl :asdf))
(in-package :cl-numerics-scalar-asd)

(defsystem cl-numerics-scalar
  :version "0.1"
  :author "Alexey Cherkaev"
  :license "GPL3"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-numerics-scalar"))))
  :description "Numerical funcions on (mostly) scalars"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-numerics-scalar-test))))
