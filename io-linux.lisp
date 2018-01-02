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

(defun style->attr (style)
  (case style
    (:black   (charms/ll:color-pair +black+))
    (:red     (charms/ll:color-pair +red+))
    (:green   (charms/ll:color-pair +green+))
    (:yellow  (charms/ll:color-pair +yellow+))
    (:blue    (charms/ll:color-pair +blue+))
    (:magenta (charms/ll:color-pair +magenta+))
    (:cyan    (charms/ll:color-pair +cyan+))
    (:white   (charms/ll:color-pair +white+))
    (:bold charms/ll:A_BOLD)
    (otherwise (error (format nil "unknown style: ~A" style)))))

(defun scr-format-styled (styles &rest args)
  (if (null styles)
      (charms/ll:addstr (apply #'format (append '(nil) args)))
    (progn
      (charms/ll:attron (style->attr (car styles)))
      (apply #'scr-format-styled (cons (cdr styles) args))
      (charms/ll:attroff (style->attr (car styles))))))

(defun scr-format-reverse (&rest args)
  (charms/ll:attron charms/ll:A_REVERSE)
  (charms/ll:addstr (apply #'format (append '(nil) args)))
  (charms/ll:attroff charms/ll:A_REVERSE))

;; () -> sexp
(defun read-command-char ()
  (let ((c (charms/ll:getch)))
    (case c
      (charms/ll:KEY_UP    (setf c (char-code #\w)))
      (charms/ll:KEY_LEFT  (setf c (char-code #\a)))
      (charms/ll:KEY_DOWN  (setf c (char-code #\s)))
      (charms/ll:KEY_RIGHT (setf c (char-code #\d))))
    (if (and (<= 0 c 127)) ; ascii range
        (let ((char (code-char c)))
          (if (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9))
              (read-from-string (format nil "~C" (code-char c)))
            nil))
      nil)))

(defun read-character ()
  (let ((c (charms/ll:getch)))
    (case c
      (charms/ll:KEY_UP    #\w)
      (charms/ll:KEY_LEFT  #\a)
      (charms/ll:KEY_DOWN  #\s)
      (charms/ll:KEY_RIGHT #\d)
      (otherwise (code-char c)))))

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


(defconstant +black+   0)
(defconstant +red+     1)
(defconstant +green+   2)
(defconstant +yellow+  3)
(defconstant +blue+    4)
(defconstant +magenta+ 5)
(defconstant +cyan+    6)
(defconstant +white+   7)

(defun init-charms ()
  (charms/ll:initscr)
  (charms/ll:clearok charms/ll:*stdscr* 1)
  (charms/ll:scrollok charms/ll:*stdscr* 1)
  (charms/ll:keypad charms/ll:*stdscr* 1)
  (charms/ll:raw)
  (charms/ll:noecho)
  (unless (= 1 (charms/ll:has-colors))
    (print "Error: color support not available")
    (sb-ext:exit))
  (charms/ll:start-color)
  (charms/ll:init-pair +black+   charms/ll:COLOR_BLACK   charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +red+     charms/ll:COLOR_RED     charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +green+   charms/ll:COLOR_GREEN   charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +yellow+  charms/ll:COLOR_YELLOW  charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +blue+    charms/ll:COLOR_BLUE    charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +magenta+ charms/ll:COLOR_MAGENTA charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +cyan+    charms/ll:COLOR_CYAN    charms/ll:COLOR_BLACK)
  (charms/ll:init-pair +white+   charms/ll:COLOR_WHITE   charms/ll:COLOR_BLACK)
  (exit-hooks:add-exit-hook #'charms/ll:endwin))

;;移動先選択
(defun map-move (map p)
  (unless (or *battle?* (= *end* 2))
    ;;(show-fog-map map p)
    (show-map map p)
    (labels ((interact ()
                       (case (read-character)
                         ((#\w #\k #\8) (update-map map p -1 0))
                         ((#\s #\j #\2) (update-map map p 1 0))
                         ((#\d #\l #\6) (update-map map p 0 1))
                         ((#\a #\h #\4) (update-map map p 0 -1))
                         ((#\W #\K) (update-map-dash map p -1 0))
                         ((#\S #\J) (update-map-dash map p 1 0))
                         ((#\D #\L) (update-map-dash map p 0 1))
                         ((#\A #\H) (update-map-dash map p 0 -1))
                         (#\q (use-heal p))
                         (#\i (show-item p))
                         (#\r (setf *end* 2))
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

