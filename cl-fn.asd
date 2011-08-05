(defpackage :cl-fn.asd (:use :cl :asdf))
(in-package :cl-fn.asd)

(defsystem :cl-fn
  :version "0.1"
  :author "Manabu Takayama <learn.libra@gmail.com>"
  :licence "MIT License"
  :serial t
  :components ((:file "packages")
               (:file "cl-fn")))
