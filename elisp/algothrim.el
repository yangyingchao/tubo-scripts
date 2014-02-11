;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;
;;; File: algothrim.el
;;; Author: YangYingchao <yangyingchao@gmail.com>
;;;
;;; Time-stamp: <2014-02-11 by Yang,Ying-chao>
;;;
;;;
;;;

(defun random-list (n &optional MAX &optional unique)
  "Generate a random list whose length is n."
  (let ((limit (if MAX MAX most-positive-fixnum))
        lst)
    (if unique
        (let ((tbl (make-hash-table :test 'equal :size n))
              r)
          (while (< (hash-table-count tbl) n)
            (print (hash-table-count tbl))
            (puthash (random limit) 0 tbl ))
          (maphash (lambda (k v)
                     (setq lst (cons k lst))) tbl))
      (while (> n 0)
        (setq lst (cons (random limit) lst)
              n (1- n))))
    lst))

(defun binary_search (array target)
  "Perform binary search for target in array"
  )

(provide 'algothrim)
;;;;; algothrim.el ends here
