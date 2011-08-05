(in-package :cl-user)

(defpackage :cl-fn.fn
  (:use :cl)
  (:export :fn))

(defpackage :cl-fn.alias
  (:use :cl)
  (:export :defalias))

(defpackage :cl-fn.comp
  (:use :cl)
  (:export :compose :conjoin :disjoin))

(defpackage :cl-fn.pa
  (:use :cl)
  (:export :curry :rcurry))

(defpackage :cl-fn.flip
  (:use :cl)
  (:export :flip))

(defpackage :cl-fn
  (:use :cl :cl-fn.fn :cl-fn.alias :cl-fn.comp :cl-fn.pa :cl-fn.flip)
  (:export :fn :defalias :compose :conjoin :disjoin :curry :rcurry :flip))
