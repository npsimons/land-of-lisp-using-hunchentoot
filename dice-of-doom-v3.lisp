(load "dice-of-doom-v2.lisp")
(load "web-server.lisp")
(load "svg.lisp")

(defparameter *board-width* 900)
(defparameter *board-height* 500)
(defparameter *board-scale* 64)
(defparameter *top-offset* 3)
(defparameter *dice-scale* 40)
(defparameter *dot-size* 0.05)
(defparameter *die-colors* '((255 63 63) (63 63 255)))
(defparameter *current-game-tree* nil)
(defparameter *from-tile* nil)

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

(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop for y below *board-length*
     do (loop for x below *board-length*
           for position = (+ x (* *board-length* y))
           for hex = (aref board position)
           for xx = (* *board-scale* (+ (* 2 x) (- *board-length* y)))
           for yy = (* *board-scale* (+ (* y 0.7) *top-offset*))
           for color = (brightness (nth (first hex) *die-colors*)
                                   (* -15 (- *board-length* y)))
           do (if (or (member position legal-tiles) (eql position chosen-tile))
                  (tag g ()
                    (tag a ("xlink:href" (make-game-link position))
                      (draw-tile-svg x y position hex xx yy color chosen-tile)))
                  (draw-tile-svg x y position hex xx yy color chosen-tile)))))

(defun make-game-link (position)
  (format nil "/game.html?chosen=~a" position))

(defun dice-of-doom-request-handler (path header parameters)
  (if (equal path "game.html")
      (progn (format t "HTTP/1.1 200 OK")
             (format t "Content-type: text/html; charset=UTF-8~%~%")
             (princ "<!doctype html>")
             (tag center ()
               (princ "Welcome to DICE OF DOOM!")
               (tag br ())
               (let ((chosen (assoc 'chosen parameters)))
                 (when (or (not *current-game-tree*) (not chosen))
                   (setf chosen nil)
                   (web-initialize))
                 (cond ((lazy-null (caddr *current-game-tree*))
                        (web-announce-winner (cadr *current-game-tree*)))
                       ((zerop (car *current-game-tree*))
                        (web-handle-human
                         (when chosen
                           (read-from-string (cdr chosen)))))
                       (t (web-handle-computer))))
               (tag br ())
               (draw-dice-of-doom-page *current-game-tree* *from-tile*)))
      (princ "Sorry, I don't know that page.")))

(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *current-game-tree* (game-tree (generate-board) 0 0 t)))

(defun web-announce-winner  (board)
  (fresh-line)
  (let ((winners (winners board)))
    (if (> (length winners) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter winners))
        (format t "The winner is ~a" (player-letter (car winners)))))
  (tag a (href "game.html")
    (princ " play again")))

(defun web-handle-human (position)
  (cond ((not position) (princ "Please choose a hex to move from:"))
        ((eq position 'pass) (setf *current-game-tree*
                                   (cadr (lazy-car (caddr *current-game-tree*))))
         (princ "Your reinforcements have been placed.")
         (tag a (href (make-game-link nil))
           (princ "continue")))
        ((not *from-tile*) (setf *from-tile* position)
         (princ "Now choose a destination:"))
        ((eq position *from-tile*) (setf *from-tile* nil)
         (princ "Move cancelled."))
        (t (setf *current-game-tree*
                 (cadr (lazy-find-if (lambda (move)
                                       (equal (car move)
                                              (list *from-tile* position)))
                                     (caddr *current-game-tree*))))
           (setf *from-tile* nil)
           (princ "You may now ")
           (tag a (href (make-game-link 'pass))
             (princ "pass"))
           (princ " or make another move:"))))

(defun web-handle-computer ()
  (setf *current-game-tree* (handle-computer *current-game-tree*))
  (princ "The computer has moved. ")
  (tag script ()
    (princ
     "window.setTimeout('window.location=\"game.html?chosen=NIL\"',5000)")))

(defun draw-dice-of-doom-page (tree selected-tile)
  (svg *board-width*
      *board-height*
    (draw-board-svg (cadr tree)
                    selected-tile
                    (take-all (if selected-tile
                                  (lazy-mapcar
                                   (lambda (move)
                                     (when (eql (caar move)
                                                selected-tile)
                                       (cadar move)))
                                   (caddr tree))
                                  (lazy-mapcar #'caar (caddr tree)))))))
