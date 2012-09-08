(defpackage :cl-fn.asd (:use :cl :asdf))
(in-package :cl-fn.asd)

(defsystem :cl-fn
  :version "0.2"
  :author "Manabu Takayama <learn.libra@gmail.com>"
  :licence "MIT License"
  :serial t
  :components ((:file "packages")
               (:file "cl-fn")))

(defsystem :cl-fn-test
  :depends-on (:cl-fn :fiveam)
  :components ((:module "t"
                        :serial t
                        :components ((:file "packages")
                                     (:file "suites")
                                     (:file "sapply")))))

(defmethod perform ((o test-op) (c (eql (find-system :cl-fn))))
  (asdf:load-system :cl-fn-test)
  (funcall (intern "RUN!" :5am) (intern "ALL" :cl-fn.test)))
