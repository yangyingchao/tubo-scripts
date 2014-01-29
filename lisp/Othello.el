;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;
;;; File: test.el
;;; Author: YangYingchao <yangyingchao@gmail.com>
;;;
;;; Time-stamp: <2014-01-29 by Yang,Ying-chao>
;;;
;;;
;;;

(defvar o-board nil "nil")

(defconst o-size 8 "size of board")
(defconst o-steps
  (list (cons 1 1) (cons 1 -1) (cons -1 -1) (cons -1 1))
  "Descriptions."
  )

(defun o-str-pos (p)
  "description"
  (if p
      (format "%d:%d" (car p) (cdr p))
    "nil"))


(defun o-pos+ (p1 p2)
  "description"
  (cons (+ (car p1) (car p2))
        (+ (cdr p1) (cdr p2))))


(defun o-distance (p1 p2)
  "description"
  (if (= (car p1) (car p2))
      (abs (- (cdr p1) (cdr p2)))
    (abs (- (car p1) (car p2)))))

(defmacro o-loop-for-size (outer-func &rest args)
  "description"
  `(let ((i 0) (j 0))
    (while (< i o-size)
      (if ,outer-func
          (funcall ,outer-func))
      (setq j 0)
      (while (< j o-size)
        ,@args
        (setq j (1+ j)))
      (setq i (1+ i)))))

(defun o-test-fn (cl cr)
  ""
  (equal cl cr))

(defun o-hash-fn (cl)
  (+ (* (car cl) o-size) (cdr cl)))


(define-hash-table-test 'o-hash 'o-test-fn 'o-hash-fn)

(defvar o-table nil "nil")

(setq o-table (make-hash-table :test 'o-hash :size 64))

(defun o-board-init ()
  "Initialize play board"
  (o-loop-for-size nil
   (puthash (cons i j) nil o-table))

  (puthash (cons 3 3) 'b o-table)
  (puthash (cons 3 4) 'b o-table)
  (puthash (cons 3 5) 'b o-table)
  (puthash (cons 4 3) 'w o-table)
  (puthash (cons 4 4) 'w o-table)
  (puthash (cons 4 5) 'w o-table)


  ;; (puthash (cons 3 3) 'b o-table)
  ;; (puthash (cons 3 4) 'w o-table)
  ;; (puthash (cons 4 3) 'w o-table)
  ;; (puthash (cons 4 4) 'b o-table)
  )

(defun o-board-draw ()
  "Draw play board"
  (with-current-buffer (get-buffer-create "*test*")
    (erase-buffer)
    (goto-char (point-min))
    (o-loop-for-size
     (insert "\n")
     (setq c (gethash (cons i j) o-table))
     (cl-case c
       ('b (insert "B"))
       ('w (insert "W"))
       (t (insert ".")))
     (insert " "))))

(defun o-get-best-step (c &optional depth)
  "get next best stop for color c.
It uses Maximum Disc Strategy which is very bad...
depth is not supported for now."
  (let ((mx 0)
        (tx 0)
        c-pos cc
        fn)
    (o-loop-for-size
     nil
     (setq tx 0)
     (setq c-pos (cons i j))
     (setq cc (gethash c-pos o-table))
     (when (not cc)
       (dolist  (step o-steps)
         (let* ((n-pos (o-pos+ c-pos step))
                (nc (gethash n-pos o-table)))
           (while (and nc (not (equal nc c)))
             (setq n-pos (o-pos+ n-pos step)
                   nc (gethash n-pos o-table)))
           (when (equal nc c)
             (setq tx (+ tx (o-distance n-pos c-pos)))))))
     (when (> tx mx)
       (message "Updating max: (%s)%d -- (%s)%d"
                (o-str-pos c-pos) tx
                (o-str-pos fn)  mx)
       (setq mx tx
             fn c-pos)))
    fn))

(progn
  (o-board-init)
  (o-board-draw)
  (print (o-get-best-step 'w))
  )

;;;;; test.el ends here
