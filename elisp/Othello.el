;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;
;;; File: test.el
;;; Author: YangYingchao <yangyingchao@gmail.com>
;;;
;;; Time-stamp: <2014-01-30 by Yang,Ying-chao>
;;;
;;;
;;;

(defvar o-size 8 "size of board")

(defconst o-steps
  (list (cons 0 1) (cons 1 0) (cons 0 -1) (cons -1 0)
        (cons 1 1) (cons 1 -1) (cons -1 -1) (cons -1 1))
  "Descriptions.")

(defun o-str-pos (p) "description"
  (if p (format "%d:%d" (car p) (cdr p)) "nil"))


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

(defvar o-table nil "nil")

(define-hash-table-test 'o-hash 'o-test-fn 'o-hash-fn)
(setq o-table (make-hash-table :test 'o-hash :size 64))

(defun o-board-init ()
  "Initialize play board, for test."
  (o-loop-for-size nil
                   (puthash (cons i j) nil o-table))

  (puthash (cons 3 3) 'b o-table)
  (puthash (cons 3 4) 'w o-table)
  (puthash (cons 4 3) 'w o-table)
  (puthash (cons 4 4) 'b o-table)

  (setq o-index 1
        o-logs ""))

(defvar o-debug nil "nil")

(defun o-board-draw ()
  "Draw play board"
  (let ((buffer (get-buffer-create (if o-debug (format "*Othello-%d*" o-index) "*Othello*")))
        c)
    (with-current-buffer buffer
      (erase-buffer)
      (goto-char (point-min))
      (o-loop-for-size
       (insert "\n")
       (let ((c (gethash (cons i j) o-table)) )
         (cl-case c
           ('b (insert "B"))
           ('w (insert "W"))
           (t (insert ".")))
         )
       (insert " ")
       (goto-char (point-max)))
      (insert (format "\nSteps:\n%s" o-logs))
      (goto-char (point-min)))))

;;; Strategies begins here ...

(defun o-evaluate-postion (c pos table)
  "Evaluate specified position, find out how many discs it will earn."
  (let ((tx 0)
        (cc (gethash pos table))
        step)
    (when (not cc)
      (dolist  (step o-steps)
        (let* ((t-pos (o-pos+ pos step))
               (nc (gethash t-pos table))
               n-pos)
          (while (and nc (not (equal nc c)))
            (setq n-pos t-pos
                  t-pos (o-pos+ t-pos step)
                  nc (gethash n-pos table)))
          (when (and n-pos (equal (gethash n-pos table) c))
            (setq tx (+ tx (o-distance n-pos pos)))))))
    ;; (message "Returning %d for %s -- %s" tx (o-str-pos pos) (symbol-name c))
    tx))

(defun o-strategy-simple (c table depth)
  "Most simple strategy
Just find the first possible persition."
  (catch 'TAG
    (o-loop-for-size
     nil
     (let* ((n-pos (cons i j))
            (tx (o-evaluate-postion c n-pos table)))
       (if (> tx 0)
           (throw 'TAG n-pos) nil)))))

(defun o-strategy-max-disc (c table depth)
  "Maximum Disc strategy."
  (let ((mx 0)
        (tx 0)
        c-pos
        f-pos)
    (o-loop-for-size
     nil
     (setq c-pos (cons i j))
     (when (> (setq tx (o-evaluate-postion c c-pos (copy-hash-table o-table))) mx)
       (message "Updating max: (%s)%d -- (%s)%d"
                (o-str-pos c-pos) tx
                (o-str-pos f-pos)  mx)
       (setq mx tx
             f-pos c-pos)))
    f-pos))

;;;  Strategies ends here.

(defun o-get-best-step (c &optional o-strategy &optional depth)
  "get next best stop for color c.
It uses Maximum Disc Strategy which is very bad...
depth is not supported for now."
  (let ((strategy (if o-strategy o-strategy 'o-strategy-simple)))
    (funcall strategy c (copy-hash-table o-table) 0)))


(defun o-update-board (pos c)
  "Update current board"
  ;; Check if disc can be put in this position.
  (let (step n-pos)
    (puthash pos c o-table)
    (o-loop-for-size
     nil
     (dolist  (step o-steps)
       (let* ((n-pos (o-pos+ pos step))
              (nc (gethash n-pos o-table)))
         ;; Loop to check if can reverse.
         (while (and nc (not (equal nc c)))
           (setq n-pos (o-pos+ n-pos step)
                 nc (gethash n-pos o-table)))
         (when (equal nc c)
           ;; do update.
           (setq n-pos (o-pos+ pos step)
                 nc (gethash n-pos o-table))
           (while (and nc (not (equal nc c)))
             (puthash n-pos c o-table)
             (setq n-pos (o-pos+ n-pos step)
                   nc (gethash n-pos o-table)))))))))

(defvar o-global-timer nil "nil")
(defvar o-logs "" "nil")
(defvar o-index 0 "nil")

(defun o-calculate-result ()
  "description"
  (let ((ws 0) (bs 0 ))
    (o-loop-for-size
     nil
     (cl-case (gethash (cons i j) o-table)
       ('b (setq bs (1+ bs)))
       ('w (setq ws (1+ ws)))))
    (cons ws bs)))

(defun o-thello-self-play (c)
  "Function called by timer to play with itself"
  (let (pos stop)
    (if (not c) (setq stop t)
      (setq pos (o-get-best-step c
                                 (if (equal c 'b)
                                     'o-strategy-max-disc ;; Black uses max-disc
                                   'o-strategy-simple)))  ;; white uses simple.
      (setq o-logs
            (concat (format "\n%02d --- Color: %s, BestPos: %s"
                            o-index (symbol-name c) (o-str-pos pos))
                    o-logs))
      (setq o-index (1+ o-index))

      (if (not pos)
          (setq stop t)
        (o-update-board pos c)
        (o-board-draw)
        (setq c (if (eq c 'b) 'w 'b))))
    (if (not stop)
        (setq o-global-timer (run-at-time "0.001 sec" nil 'o-thello-self-play c))

      ;; Last step, cancel timer and calculate statistics.
      (message "STOP!!!!!")
      (if o-global-timer
          (cancel-timer o-global-timer))
      (othello-print-result))))

(defun othello-print-result ()
  "Print result"
  (interactive)
  (message "Calculating results...")
  (let* ((res (o-calculate-result))
         (ws (car res))
         (bs (cdr res)))
    (message "Black: %d, White: %d, %s!"
             bs ws (if (= ws bs)
                       "Draw"
                     (if (> ws bs) "White wins" "Black wins")))))

(defun othello-start (&optional sz)
  "Start othello game"
  (interactive)
  (if (and (integerp sz)
           (> sz 2))
      (setq o-size sz))
  (if o-global-timer
      (cancel-timer o-global-timer))
  (with-current-buffer (get-buffer-create "*Othello*")
    (set-window-buffer (get-buffer-window)  (current-buffer)))
  (o-board-init)
  (o-board-draw)
  (o-thello-self-play 'b))

;;;;; test.el ends here
