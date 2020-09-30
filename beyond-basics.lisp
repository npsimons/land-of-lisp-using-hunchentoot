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
