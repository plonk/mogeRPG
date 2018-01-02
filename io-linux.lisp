(ql:quickload :cl-charms)
(ql:quickload :exit-hooks)
(ql:quickload :flexi-streams)

(defun scr-fresh-line ()
  (let ((x (charms/ll:getcurx charms/ll:*stdscr*)))
    (if (/= 0 x)
        (charms/ll:addstr (format nil "~%")))))

(defun scr-princ (obj)
  (charms/ll:addstr (format nil "~A" obj)))

(defun scr-format (&rest args)
  (charms/ll:addstr (apply #'format (append '(nil) args))))

(defun scr-format-reverse (&rest args)
  (charms/ll:attron charms/ll:A_REVERSE)
  (charms/ll:addstr (apply #'format (append '(nil) args)))
  (charms/ll:attroff charms/ll:A_REVERSE))

;; () -> sexp
(defun read-command-char ()
  (let ((c (charms/ll:getch)))
    (cond
      ((= c charms/ll:KEY_UP)    (setf c (char-code #\w)))
      ((= c charms/ll:KEY_LEFT)  (setf c (char-code #\a)))
      ((= c charms/ll:KEY_DOWN)  (setf c (char-code #\s)))
      ((= c charms/ll:KEY_RIGHT) (setf c (char-code #\d))))
    (if (and (<= 0 c) (<= c 127)) ; ascii range
        (let ((char (code-char c)))
          (if (or
               (and (char<= #\a char) (char<= char #\z))
               (and (char<= #\A char) (char<= char #\Z))
               (and (char<= #\0 char) (char<= char #\9)))
              (read-from-string (format nil "~C" (code-char c)))
	      nil))
	nil)))

(defun gets ()
  (let ((buf (make-array 0 :adjustable t :fill-pointer 0)))
    (labels
        ((add-char ()
                   (let ((code (charms/ll:getch)))
                     (cond
		       ((= code 10)
			(charms/ll:scrl 1)
			(flexi-streams:octets-to-string buf :external-format :utf-8))
		       (t
			 (vector-push-extend code buf)
                         (add-char))))))
      (add-char))))

;; () -> sexp
(defun read-command-line ()
  (read-from-string (gets)))

;; () -> string
(defun read-string ()
  (gets))

(defun init-charms ()
  (charms/ll:initscr)
  (charms/ll:clearok charms/ll:*stdscr* 1)
  (charms/ll:scrollok charms/ll:*stdscr* 1)
  (charms/ll:keypad charms/ll:*stdscr* 1)
  (charms/ll:raw)
  (charms/ll:noecho)
  (exit-hooks:add-exit-hook #'charms/ll:endwin))

;;移動先選択
(defun map-move (map p)
  (unless (or *battle?* (= *end* 2))
    ;;(show-fog-map map p)
    (show-map map p)
    (labels ((interact ()
		     (case (read-command-char)
			   ((w k 8) (update-map map p -1 0))
			   ((s j 2) (update-map map p 1 0))
			   ((d l 6) (update-map map p 0 1))
			   ((a h 4) (update-map map p 0 -1))
			   (q (use-heal p))
			   (i (show-item p))
			   (r (setf *end* 2))
			   (otherwise
			    (interact)))))
	  (interact))
    (map-move map p)))

(defun show-map-key ()
  (scr-format "[q]薬を使う [r]終わる: ~%"))

(defun gamen-clear ()
  (charms/ll:clear))

(defun endo-win ()
  (charms/ll:endwin))

