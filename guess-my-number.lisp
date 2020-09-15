#! /usr/bin/env clisp

(defparameter *small* 1)

(defparameter *big* 100)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))

(defun square (n)
  (* n n))

(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

(defvar *number-was-odd* nil)

(defun pudding-eater (person)
  (cond ((eq person 'henry)
         (setf *arch-enemy* 'stupd-lisp-alien)
         '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny)
         (setf *arch-enemy* 'useless-old-johnny)
         '(i hope you choked on my pudding johnny))
        (t
         '(why you eat my pudding stranger ?))))

(defun pudding-eater (person)
  (case person
    ((henry)
     (setf *arch-enemy* 'stupd-lisp-alien)
     '(curse you lisp alien - you ate my pudding))
    (('johnny)
     (setf *arch-enemy* 'useless-old-johnny)
     '(i hope you choked on my pudding johnny))
    (otherwise
     '(why you eat my pudding stranger ?))))
