(load "dice-of-doom-v2.lisp")
(load "web-server.lisp")
(load "svg.lisp")

(defparameter *board-width* 900)
(defparameter *board-height* 500)
(defparameter *board-scale* 64)
(defparameter *top-offset* 3)
(defparameter *dice-scale* 40)
(defparameter *dot-size* 0.05)

(defun draw-die-svg (x-position y-position color)
  (labels ((calculate-point (point)
             (cons (+ x-position (* *dice-scale* (car point)))
                   (+ y-position (* *dice-scale* (cdr point)))))
           (f (polygon-points color)
             (polygon (mapcar #'calculate-point polygon-points) color)))
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75))
       (brightness color 40))
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))
       color)
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))
       (brightness color -40))
    (mapc (lambda (x y)
            (polygon (mapcar (lambda (xx yy)
                               (calculate-point (cons (+ x (* xx *dot-size*))
                                                      (+ y (* yy *dot-size*)))))
                             '(-1 -1 1 1)
                             '(-1 1 1 -1))
                     '(255 255 255)))
          '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
          '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
            -0.35 -0.05 -0.45 -0.15 -0.45 -0.05))))

(defun draw-tile-svg (x y position hex xx yy color chosen-tile)
  (loop for z below 2
     do (polygon (mapcar (lambda (point)
                           (cons (+ xx (* *board-scale* (car point)))
                                 (+ yy (* *board-scale*
                                          (+ (cdr point) (* (- 1 z) 0.1))))))
                         '((-1 . -0.2) (0 . -0.5) (1 . -0.2)
                           (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
                 (if (eql position chosen-tile)
                     (brightness color 100)
                     color)))
  (loop for z below (second hex)
     do (draw-die-svg (+ xx
                         (* *dice-scale*
                            0.3
                            (if (oddp (+ x y z))
                                -0.3
                                0.3)))
                      (- yy (* *dice-scale* z 0.8)) color)))
