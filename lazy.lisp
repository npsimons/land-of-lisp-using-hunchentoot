(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defparameter *integers*
  (labels ((f (n)
             (lazy-cons n (f (1+ n)))))
    (f 1)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (list)
  (lazy (when list
          (cons (car list) (make-lazy (cdr list))))))

(defun take (count list)
  (unless (or (zerop count) (lazy-null list))
    (cons (lazy-car list) (take (1- count) (lazy-cdr list)))))

(defun take-all (list)
  (unless (lazy-null list)
    (cons (lazy-car list) (take-all (lazy-cdr list)))))

(defun lazy-mapcar (function list)
  (lazy (unless (lazy-null list)
          (cons (funcall function (lazy-car list))
                (lazy-mapcar function (lazy-cdr list))))))

(defun lazy-mapcan (function list)
  (labels ((f (list-cursor)
             (if (lazy-null list-cursor)
                 (force (lazy-mapcan function (lazy-cdr list)))
                 (cons (lazy-car list-cursor) (lazy (f (lazy-cdr list-cursor)))))))
    (lazy (unless (lazy-null list)
            (f (funcall function (lazy-car list)))))))

(defun lazy-find-if (function list)
  (unless (lazy-null list)
    (let ((x (lazy-car list)))
      (if (funcall function x)
          x
          (lazy-find-if function (lazy-cdr list))))))

(defun lazy-nth (n list)
  (if (zerop n)
      (lazy-car list)
      (lazy-nth (1- n) (lazy-cdr list))))
