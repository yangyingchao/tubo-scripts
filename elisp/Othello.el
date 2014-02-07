;;; -*- emacs-lisp -*- -*- coding: utf-8; -*-
;;;
;;; File: test.el
;;; Author: YangYingchao <yangyingchao@gmail.com>
;;;
;;; Time-stamp: <2014-02-07 by Yang,Ying-chao>
;;;
;;;
;;;
(require 'cl)

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
  (clrhash o-table)
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
     (setq c-pos (cons i j)
           tx (o-evaluate-postion c c-pos (copy-hash-table o-table)))
     (if (< tx mx)
         nil ;; skip this postion.
       (when (> tx 0)
         (when (> tx mx)
           ;; (message "Updating max: (%s)%d -- (%s)%d"
           ;;          (o-str-pos c-pos) tx
           ;;          (o-str-pos (car f-pos))  mx)
           (setq mx tx
                 f-pos nil))
         (setq f-pos (cons c-pos f-pos)))))
    (nth (random (length f-pos)) f-pos)))

(defun o-generate-postion-weight ()
  (let* ((tbl (make-hash-table :test 'o-hash :size 64))
        (x0 (/ (1- o-size) 2))
        (y0 x0)
        t-list)
    (o-loop-for-size
     nil
     (puthash (cons i j) (floor (* -10 (sqrt (+ (* (- i x0) (- i x0))
                                                (* (- j y0) (- j y0))))))
              tbl)
     )
    (puthash (cons 0 0) 64 tbl)
    (puthash (cons 0 (1- o-size)) 64 tbl)
    (puthash (cons (1- o-size) 0) 64 tbl)
    (puthash (cons (1- o-size) (1- o-size)) 64 tbl)
    tbl))

(defvar o-ai-1-db nil "nil")
(defvar o-ai-1-pos-list nil "nil")

(defun load-ai-1-db ()
  "Load db for ai-1"
  (setq o-ai-1-pos-list  nil)
  (setq o-ai-1-db
        (if (not (file-exists-p "~/Work/tubo-scripts/elisp/Othello-AI-1.el"))
            (o-generate-postion-weight)
          (with-temp-file (make-temp-file "Othello-AI-1")
            (insert-file-contents "Othello-AI-1.el")
            (read (set-marker (make-marker) 0 (current-buffer)))))))

(defun save-ai-1-db (win)
  "description"
  (merge-ai-1-db win)
  (with-temp-file  "~/Work/tubo-scripts/elisp/Othello-AI-1.el"
    (erase-buffer)
    (print o-ai-1-db (current-buffer))))

(defun merge-ai-1-db (win)
  "description"
  (let ((val (if win 1 -1)) )
    (dolist (pos o-ai-1-pos-list)
      (puthash pos (+ val (gethash pos o-ai-1-db)) o-ai-1-db))))

(defun o-strategy-ai-1 (c table depth)
  "AI-1
It simple get a list of possible positions and check data base to get its weight"
  (if (not o-ai-1-db)
      (load-ai-1-db))
  (let ((tx 0)
        (c-weight 0)
        (f-weight most-negative-fixnum)
        c-pos
        f-pos)
    (o-loop-for-size
     nil
     (setq c-pos (cons i j)
           tx (o-evaluate-postion c c-pos (copy-hash-table o-table)))
     (when (> tx 0)
       (setq c-weight (gethash c-pos o-ai-1-db))
       (when (> c-weight f-weight)
         (setq f-weight c-weight
               f-pos c-pos)
         (add-to-list 'o-ai-1-pos-list f-pos))))
    ;; (message "Pos: %s, weight: %d" (o-str-pos f-pos) f-weight)
    f-pos))

(defun o-get-potential-plist (c table)
  (let ((tx 0)
        c-pos
        pos-list)
    (o-loop-for-size
     nil
     (setq c-pos (cons i j)
           tx (o-evaluate-postion c c-pos (copy-hash-table o-table)))
     (if (> tx 0)
         (setq pos-list (cons (cons c-pos tx) pos-list))))
    pos-list))

(defmacro oc-rev (c)
  "get reverse color"
  `(if (eq ,c 'b) 'w 'b))


(defun o-strategy-minimax (c table depth)
  "AI using minmax algorithm"
  ;;   function minimax(node, depth, maximizingPlayer)
  ;;     if depth = 0 or node is a terminal node
  ;;         return the heuristic value of node
  ;;     if maximizingPlayer
  ;;         bestValue := -∞
  ;;         for each child of node
  ;;             val := minimax(child, depth - 1, FALSE))
  ;;             bestValue := max(bestValue, val);
  ;;         return bestValue
  ;;     else
  ;;         bestValue := +∞
  ;;         for each child of node
  ;;             val := minimax(child, depth - 1, TRUE))
  ;;             bestValue := min(bestValue, val);
  ;;         return bestValue

  ;; (* Initial call for maximizing player *)
  minimax(origin, depth, TRUE)
  minimax(origin, depth, TRUE)

  (o-max c table depth))

;;;  Strategies ends here.

(defun o-get-best-step (c &optional o-strategy &optional depth)
  "get next best stop for color c.
It uses Maximum Disc Strategy which is very bad...
depth is not supported for now."
  (let ((strategy (if o-strategy o-strategy 'o-strategy-simple)))
    (funcall strategy c (copy-hash-table o-table)
             (if depth depth 0))))


(defun o-update-board (pos c &optional table)
  "Update current board"
  ;; Check if disc can be put in this position.
  (let ((tbl (if table table o-table))
        step n-pos)
    (puthash pos c tbl)
    (o-loop-for-size
     nil
     (dolist  (step o-steps)
       (let* ((n-pos (o-pos+ pos step))
              (nc (gethash n-pos tbl)))
         ;; Loop to check if can reverse.
         (while (and nc (not (equal nc c)))
           (setq n-pos (o-pos+ n-pos step)
                 nc (gethash n-pos tbl)))
         (when (equal nc c)
           ;; do update.
           (setq n-pos (o-pos+ pos step)
                 nc (gethash n-pos tbl))
           (while (and nc (not (equal nc c)))
             (puthash n-pos c tbl)
             (setq n-pos (o-pos+ n-pos step)
                   nc (gethash n-pos tbl)))))))))

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

(defun o-thello-self-play (c &optional i)
  "Function called by timer to play with itself, always returns next color"
  (defun o-self-get-next-step (c)
    "description"
    (o-get-best-step c
                     (if (equal c 'b)
                         'o-strategy-max-disc ;; Black uses max-disc
                       'o-strategy-minimax)  ;; white uses simple.
                     1))
  (let (pos stop)
    (if (not c) (setq stop t)
      (setq pos (o-self-get-next-step c))
      (setq o-logs
            (concat (format "\n%02d --- Color: %s, BestPos: %s"
                            o-index (symbol-name c) (o-str-pos pos))
                    o-logs))
      (setq o-index (1+ o-index))

      (if (not pos)
          (let* ((tc (if (eq c 'b) 'w 'b))
                 (tpos (o-strategy-simple tc  (copy-hash-table o-table) 1)))
            (message "No place to drop: %s, trying to skip ..." (symbol-name c))
            (if tpos
                (setq c (if (eq c 'b) 'w 'b))
              (message "No place to drop for color: %s" (symbol-name tc))
              (setq stop t)))
        (o-update-board pos c)
        (o-board-draw)
        (setq c (if (eq c 'b) 'w 'b))))

    (when (not i)
      (if (not stop)
          (setq o-global-timer (run-at-time "0.0001 sec" nil 'o-thello-self-play c))

        ;; Last step, cancel timer and calculate statistics.
        (if o-global-timer
            (cancel-timer o-global-timer))
        (save-ai-1-db (othello-print-result))
        (when (< (hash-table-count o-table) (* o-size o-size))
          (with-current-buffer (get-buffer "*Othello*")
            (rename-buffer (format "*Othello-%s*"
                                   (format-time-string "%s" (current-time))))))
        (setq o-global-timer (run-at-time "2 sec" nil 'othello-start o-size))))
    c))

(defvar o-cur-col nil "nil")
(defun othello-next ()
  "description"
  (interactive)
  (when (not o-cur-col)
      (setq o-cur-col 'b)
      (setq o-size 8)
      (if o-global-timer
          (cancel-timer o-global-timer))
      (with-current-buffer (get-buffer-create "*Othello*")
        (set-window-buffer (get-buffer-window)  (current-buffer)))
      (o-board-init)
      (o-board-draw))
  (setq o-cur-col (o-thello-self-play o-cur-col t)))

(defun othello-reset ()
  "description"
  (interactive)
  (setq o-cur-col nil))

(defun othello-print-result ()
  "Print result, returns t if w wins"
  (interactive)
  (message "Calculating results...")
  (let* ((res (o-calculate-result))
         (ws (car res))
         (bs (cdr res)))
    (message "Black: %d, White: %d, %s!"
             bs ws (if (= ws bs)
                       "Draw"
                     (if (> ws bs) "White wins" "Black wins")))
    (> ws bs)))

(defun othello-start (&optional sz)
  "Start othello game"
  (interactive)
  (if (and (integerp sz)
           (> sz 2))
      (setq o-size sz))
  (load-ai-1-db)
  (if o-global-timer
      (cancel-timer o-global-timer))
  (with-current-buffer (get-buffer-create "*Othello*")
    (set-window-buffer (get-buffer-window)  (current-buffer)))
  (o-board-init)
  (o-board-draw)
  (o-thello-self-play 'b))

(defun othello-stop ()
  "description"
  (interactive)
  (if o-global-timer
      (cancel-timer o-global-timer))
  ;; (save-ai-1-db (othello-print-result))
  )

;;;;; test.el ends here
