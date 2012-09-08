(in-package :cl-fn.test)

(5am:in-suite sapply)

(5am:test sapply/sequentially-apply
  (5am:is (eq t (sequentially-apply t)))
  (5am:is (= 0 (sequentially-apply 0)))
  (5am:is (= 0 (sequentially-apply (values 0))))
  (5am:is (eq t (sequentially-apply :ignored t)))
  (5am:is (eq t (sequentially-apply :ignored (values t))))
  (5am:is (eq t (sequentially-apply t _)))
  (5am:is (= 0 (sequentially-apply 0 _)))
  (5am:is (eq t (sequentially-apply t (identity _))))
  (5am:is (= 0 (sequentially-apply 0 (identity _))))
  (5am:is (= 2 (sequentially-apply 0 (1+ _) (1+ _))))
  (5am:is (= 8 (sequentially-apply 1 (- 5 _) (* _ 2))))
  (5am:is (equal '(0 1)
                 (multiple-value-list (sequentially-apply (values 0 1)))))
  (5am:is (equal '(0 1)
                 (sequentially-apply (values 0 1)
                   (list _ _)))))