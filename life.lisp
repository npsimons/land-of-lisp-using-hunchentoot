(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)

(defparameter *plants* (make-hash-table :test #'equal))

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

(defstruct animal
  x-position
  y-position
  energy
  direction
  genes)

(defparameter *animals*
  (list (make-animal :x-position (ash *width* -1)
                     :y-position (ash *height* -1)
                     :energy 1000
                     :direction 0
                     :genes (loop repeat 8
                               collecting (1+ (random 10))))))

(defun move (animal)
  (let ((direction (animal-direction animal))
        (x-position (animal-x-position animal))
        (y-position (animal-y-position animal)))
    (setf (animal-x-position animal)
          (mod (+ x-position (cond
                               ((and (>= direction 2) (< direction 5)) 1)
                               ((or (= direction 1) (= direction 5)) 0)
                               (t -1)))
               *width*))
    (setf (animal-y-position animal)
          (mod (+ y-position (cond
                               ((and (>= direction 0) (< direction 3)) -1)
                               ((and (>= direction 4) (< direction 7)) 1)
                               (t 0)))
               *height*))
    (decf (animal-energy animal))))

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x)
               (let ((x-new (- x (car genes))))
                 (if (< x-new 0)
                     0
                     (1+ (angle (cdr genes) x-new))))))
      (setf (animal-direction animal)
            (mod (+ (animal-direction animal) (angle (animal-genes animal) x))
                 8)))))

(defun eat (animal)
  (let ((position (cons (animal-x-position animal) (animal-y-position animal))))
    (when (gethash position *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash position *plants*))))

(defparameter *reproduction-energy-minimum* 200)

(defun reproduce (animal)
  (let ((energy (animal-energy animal)))
    (when (>= energy *reproduction-energy-minimum*)
      (setf (animal-energy animal) (ash energy -1))
      (let ((animal-new (copy-structure animal))
            (genes (copy-list (animal-genes animal)))
            (mutation (random 8)))
        (setf (nth mutation genes)
              (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-new) genes)
        (push animal-new *animals*)))))

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
                               (<= (animal-energy animal) 0))
                             *animals*))
  (mapc (lambda (animal)
          (turn animal)
          (move animal)
          (eat animal)
          (reproduce animal))
        *animals*)
  (add-plants))

(defun draw-world ()
  (loop for y-position below *height*
     do (progn (fresh-line)
               (princ "|")
               (loop for x-position below *width*
                  do (princ (cond ((some (lambda (animal)
                                           (and (= (animal-x-position animal) x-position)
                                                (= (animal-y-position animal) y-position)))
                                         *animals*)
                                   #\M)
                                  ((gethash (cons x-position y-position) *plants*) #\*)
                                  (t #\space))))
               (princ "|"))))

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
          (t (let ((x-position (parse-integer str :junk-allowed t)))
               (if x-position
                   (loop for i below x-position
                      do (update-world)
                      if (zerop (mod i 1000))
                      do (princ #\.))
                   (update-world))
               (evolution))))))
