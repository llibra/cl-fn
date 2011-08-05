(in-package :cl-fn.fn)

(defun convert-args (args)
  (let (ignores specials)
    (labels ((special-p (v)
               (and (symbolp v)
                    (let ((c (elt (symbol-name v) 0)))
                      (or (eql c #\$) (eql c #\*)))))
             (ignore-p (v)
               (and (symbolp v) (string= v "_")))
             (convert-arg (arg)
               (cond ((ignore-p arg)
                      (push (gensym "DUMMY") ignores)
                      (car ignores))
                     ((special-p arg)
                      (push arg specials)
                      arg)
                     (t arg))))
      (values
       (cond ((null args) nil)
             ((atom args) (list '&rest (convert-arg args)))
             (t (mapcar #'convert-arg args)))
       `(declare (ignore ,@ignores) (special ,@specials))))))

(defmacro fn (args &body body)
  (multiple-value-bind (args decls) (convert-args args)
    `(lambda ,args ,decls ,@body)))

(in-package :cl-fn.alias)

(defmacro defalias (name function-designator)
  (with-gensyms (function designator)
    `(let* ((,designator ,function-designator)
            (,function (if (functionp ,designator)
                           ,designator
                           (symbol-function ,designator))))
       (setf (symbol-function ',name) ,function)
       ',name)))

(in-package :cl-fn.comp)

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
        (reduce #'(lambda (v f) (funcall f v))
                rest
                :initial-value (apply fn1 args)))))

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
        #'(lambda (&rest args)
            (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
        #'(lambda (&rest args)
            (and (apply fn args) (apply conj args))))))

(in-package :cl-fn.pa)

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(in-package :cl-fn.flip)

(defun flip (fn x y)
  (funcall fn y x))
