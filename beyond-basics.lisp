(define-condition foo () ()
           (:report (lambda (condition stream)
                      (princ "Stop FOOing around, numbskull!" stream))))

;; The clean, functional part
(defun add-widget (database widget)
  (cons widget database))

;; The dirty, nonfunctional part
(defparameter *database* nil)

(defun main-loop ()
  (loop
     (princ "Please enter the name of a new widget: ")
     (setf *database* (add-widget *database* (read)))
     (format t "The database contains the following: ~a~%" *database*)))

(defparameter *foo* (let ((x 5))
                      (lambda ()
                        x)))

(let ((line-number 0))
  (defun my-print (x)
    (print line-number)
    (print x)
    (incf line-number)
    nil))

(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

(defun my-length (list)
  (labels ((f (list accumulator)
             (if list
                 (f (cdr list) (1+ accumulator))
                 accumulator)))
    (f list 0)))

(compile 'my-length)

(defparameter *biglist* (loop for i below 100000 collect 'x))

(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))

(defmacro let1 (variable value &body body)
  `(let ((,variable ,value))
     ,@body))

(defun add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))

;;Warning! Still contains a bug!
(defmacro split (val yes no)
  `(let1 x ,val
     (if x
         (let ((head (car x))
               (tail (cdr x)))
           ,yes)
         ,no)))

;; This function is finally safe to use.
(defmacro split (value yes no)
  (let1 g (gensym)
    `(let1 ,g ,value
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

(defun my-length (list)
  (labels ((f (list accumulator)
             (split list
                    (f tail (1+ accumulator))
                    accumulator)))
    (f list 0)))

(defmacro recurse (variables &body body)
  (let1 p (pairs variables)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))

(defun my-length (list)
  (recurse (list list
           accumulator 0)
  (split list
         (self tail (1+ accumulator))
         accumulator)))

(defun my-length (list)
  (reduce (lambda (x i)
            (1+ x))
          list
          :initial-value 0))
