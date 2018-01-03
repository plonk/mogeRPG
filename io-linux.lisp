(ql:quickload :cl-charms)
(ql:quickload :exit-hooks)
(ql:quickload :flexi-streams)

(defpackage :io
  (:use :common-lisp :charms/ll)
  (:export :endo-win
           :gamen-clear
           :gets
           :init-charms
           :read-character
           :read-command-char
           :read-command-line
           :read-string
           :scr-format
           :scr-format-reverse
           :scr-format-styled
           :scr-fresh-line
           :scr-princ
           :flush-input
           :refresh-screen))

(in-package :io)

;; カラーペア定数。
(defconstant +black+   0)
(defconstant +red+     1)
(defconstant +green+   2)
(defconstant +yellow+  3)
(defconstant +blue+    4)
(defconstant +magenta+ 5)
(defconstant +cyan+    6)
(defconstant +white+   7)
(defconstant +warning+   8)

(defun scr-fresh-line ()
  (let ((x (getcurx *stdscr*)))
    (if (/= 0 x)
        (addstr (format nil "~%")))))

(defun scr-princ (obj)
  (addstr (format nil "~A" obj)))

(defun scr-format (&rest args)
  (addstr (apply #'format (append '(nil) args))))

(defun style->attr (style)
  (case style
    (:black   (color-pair +black+))
    (:red     (color-pair +red+))
    (:green   (color-pair +green+))
    (:yellow  (color-pair +yellow+))
    (:blue    (color-pair +blue+))
    (:magenta (color-pair +magenta+))
    (:cyan    (color-pair +cyan+))
    (:white   (color-pair +white+))
    (:bold A_BOLD)
    (:warning (color-pair +warning+))
    (otherwise (error (format nil "unknown style: ~A" style)))))

(defun scr-format-styled (styles &rest args)
  (if (null styles)
      (addstr (apply #'format (append '(nil) args)))
    (progn
      (attron (style->attr (car styles)))
      (apply #'scr-format-styled (cons (cdr styles) args))
      (attroff (style->attr (car styles))))))

(defun scr-format-reverse (&rest args)
  (attron A_REVERSE)
  (addstr (apply #'format (append '(nil) args)))
  (attroff A_REVERSE))

;; () -> sexp
(defun read-command-char ()
  (let ((c (getch)))
    (cond
      ((= c KEY_UP)    (setf c (char-code #\w)))
      ((= c KEY_LEFT)  (setf c (char-code #\a)))
      ((= c KEY_DOWN)  (setf c (char-code #\s)))
      ((= c KEY_RIGHT) (setf c (char-code #\d))))
    (if (and (<= 0 c 127)) ; ascii range
        (let ((char (code-char c)))
          (if (or (char<= #\a char #\z) (char<= #\A char #\Z) (char<= #\0 char #\9))
              (read-from-string (format nil "~C" (code-char c)))
            nil))
      nil)))

(defun read-character ()
  (let ((c (getch)))
    (cond
      ((= c KEY_UP)    #\w)
      ((= c KEY_LEFT)  #\a)
      ((= c KEY_DOWN)  #\s)
      ((= c KEY_RIGHT) #\d)
      (t (code-char c)))))

(defun gets ()
  (let ((buf (make-array 0 :adjustable t :fill-pointer 0)))
    (labels
        ((add-char ()
                   (let ((code (getch)))
                     (cond
                       ((= code 10)
                        (scrl 1)
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
  (initscr)
  (clearok *stdscr* 1)
  (scrollok *stdscr* 1)
  (keypad *stdscr* 1)
  (raw)
  (noecho)
  (unless (= 1 (has-colors))
    (print "Error: color support not available")
    (sb-ext:exit))
  (start-color)
  ;;カラーペアの初期化。
  (init-pair +black+   COLOR_BLACK   COLOR_BLACK)
  (init-pair +red+     COLOR_RED     COLOR_BLACK)
  (init-pair +green+   COLOR_GREEN   COLOR_BLACK)
  (init-pair +yellow+  COLOR_YELLOW  COLOR_BLACK)
  (init-pair +blue+    COLOR_BLUE    COLOR_BLACK)
  (init-pair +magenta+ COLOR_MAGENTA COLOR_BLACK)
  (init-pair +cyan+    COLOR_CYAN    COLOR_BLACK)
  (init-pair +white+   COLOR_WHITE   COLOR_BLACK)
  (init-pair +warning+ COLOR_WHITE   COLOR_RED)
  (exit-hooks:add-exit-hook #'endwin))

(defun gamen-clear ()
  (clear))

(defun endo-win ()
  (endwin))

(defun flush-input ()
  (flushinp))

(defun refresh-screen ()
  (refresh))
