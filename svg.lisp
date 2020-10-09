(defmacro let1 (variable value &body body)
  `(let ((,variable ,value))
     ,@body))

(defmacro split (value yes no)
  (let1 g (gensym)
    `(let1 ,g ,value
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

(defun pairs (list)
  (labels ((f (list accumulator)
             (split list
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) accumulator))
                        (reverse accumulator))
                    (reverse accumulator))))
    (f list nil)))

(defun print-tag (name attribute-list closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (attribute)
          (format t " ~a=\"~a\"" (string-downcase (car attribute)) (cdr attribute)))
        attribute-list)
  (princ #\>))

(defmacro tag (name attributes &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs attributes)))
          nil)
      ,@body
      (print-tag ',name nil t)))

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink"
                   height ,height
                   width ,width)
     ,@body))

(defun brightness (color amount)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amount))))
          color))

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
                  cy (cdr center)
                  r radius
                  style (svg-style color))))

(defun polygon (points color)
  (tag polygon (points (format nil
                               "~{~a,~a ~}"
                               (mapcan (lambda (tp)
                                         (list (car tp) (cdr tp)))
                                       points))
                       style (svg-style color))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
          (random-walk (if (zerop (random 2))
                           (1- value)
                           (1+ value))
                       (1- length)))))

(with-open-file (*standard-output* "random-walk.svg"
                                   :direction :output
                                   :if-exists :supersede)
  (svg 400 200
    (loop repeat 10
       do (polygon (append '((0 . 200))
                           (loop for x from 0
                              for y in (random-walk 100 400)
                              collect (cons x y))
                           '((400 . 200)))
                   (loop repeat 3
                      collect (random 256))))))
