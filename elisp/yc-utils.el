;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;

(defconst r-match-cost
  (rx line-start (+? ascii) "TileKeys:" (* blank)
      (group (+ digit)) "," (* blank) "cost:" (* blank)
      (group (+ digit)) (* blank) (*? ascii)
      eol))

(defun yc/sum (lst)
  "description"
  (let ((sum 0))
    (if lst
        (setq sum (+ sum (car lst) (yc-sum (cdr lst)))))
    sum))


(defun yc/get-costs (content pos)
  "description"
  (let ((newpos pos)
        (result nil)
        (nK 0)
        (nC 0))
    (if content
        (when (string-match r-match-cost content pos)

          (setq nK (string-to-number (match-string 1 content))
                nC (string-to-number (match-string 2 content))
                newpos (1+ (match-end 2)))
          (setq result (append result (list (cons nK nC)) (yc/get-costs content newpos)))
          ))
    result))

(defun yc/mean (fn)
  "Compute mean value of a list"
  (let* ((content (yc/read-file-content-as-string fn))
         (lst (yc/get-costs content 0))
         (sum 0)
         (count 0))
    (dolist (x lst)
      (setq count (+ count (car x)))
      (setq sum (+ sum (cdr x)))
      )
    (/ sum count)
  ))

(progn
  (print (yc/mean "a.txt"))
  (print (yc/mean "b.txt"))
  nil
)

(defun binary-search (x lst &optional l h)
  (if (or (not (listp lst))
          (listp x))
      (message "Wrong type of arguments, usage: (binary-search  ele list)")
    (let* ((l (if l l 0))
           (h (if h h (length lst)))
           (n (/  (+ h l) 2))
           (v (nth n lst)))
      (cond
       ((or (not v) (= v x) (>= l h)) n)
       ((< x v) (binary-search x lst l (1- n)))
       ((> x v) (binary-search x lst (1+ n) h))
       )
      )
    )
  )


;; (binary-search 7341 (let ((i 0)
;;       (a nil))
;;   (while (< i 100000)
;;     (setq a (cons i a)
;;           i (1+ i))
;;       )
;;   (nreverse a)))











(provide 'yc-utils)
;;;;; yc-utils.el ends here
