#|
  This file is a part of cl-numerics-scalar project.
|#

(in-package :cl-user)
(defpackage cl-numerics-scalar-test-asd
  (:use :cl :asdf))
(in-package :cl-numerics-scalar-test-asd)

(defsystem cl-numerics-scalar-test
  :author "Alexey Cherkaev"
  :license "GPL3"
  :depends-on (:cl-numerics-scalar
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-numerics-scalar"))))
  :description "Test system for cl-numerics-scalar"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
