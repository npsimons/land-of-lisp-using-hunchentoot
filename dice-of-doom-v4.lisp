(in-package :dice-of-doom)

(load "dice-of-doom-v3.lisp")

(defparameter *number-of-players* 4)
(defparameter *die-colors* '((255 63 63)
                             (63 63 255)
                             (63 255 63)
                             (255 63 255)))
(defparameter *maximum-dice-per-space* 5)
(defparameter *ai-level* 2)

(defparameter *dice-probability* #(#(0.84 0.97 1.0 1.0)
                                   #(0.44 0.78 0.94 0.99)
                                   #(0.15 0.45 0.74 0.91)
                                   #(0.04 0.19 0.46 0.72)
                                   #(0.01 0.06 0.22 0.46)))
 
(defun attacking-moves (board current-player spare-dice)
  (labels ((player-at (position)
             (car (aref board position)))
           (dice (position)
             (cadr (aref board position))))
    (lazy-mapcan
     (lambda (source)
       (if (eq (player-at source) current-player)
           (lazy-mapcan
            (lambda (destination)
              (if (and (not (eq (player-at destination)
                                current-player))
                       (> (dice source) 1))
                  (make-lazy
                   (list (list (list source destination)
                               (game-tree (board-attack board
                                                        current-player
                                                        source
                                                        destination
                                                        (dice source))
                                          current-player
                                          (+ spare-dice (dice destination))
                                          nil)
                               (game-tree
                                (board-attack-fail board
                                                   current-player
                                                   source
                                                   destination
                                                   (dice source))
                                current-player
                                (+ spare-dice (dice destination))
                                nil))))
                  (lazy-nil)))
            (make-lazy (neighbors source)))
           (lazy-nil)))
     (make-lazy (loop for n below *board-hex-count*
                   collect n)))))

(defun board-attack-fail (board player source destination dice)
  (board-array (loop for position from 0
                  for hex across board
                  collect (if (eq position source)
                              (list player 1)
                              hex))))
(defun roll-dice (number-of-dice)
  (let ((total (loop repeat number-of-dice
                  sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled a total of ~a. " number-of-dice total)
    total))

(defun roll-against (source-dice-count destination-dice-count)
  (> (roll-dice source-dice-count) (roll-dice destination-dice-count)))

(defun pick-chance-branch (board move)
  (labels ((dice (position)
             (cadr (aref board position))))
    (let ((path (car move)))
      (if (or (null path)
              (roll-against (dice (car path))
                            (dice (cadr path))))
          (cadr move)
          (caddr move)))))

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
                 (pick-chance-branch
                  (cadr *current-game-tree*)
                  (lazy-find-if (lambda (move)
                                  (equal (car move)
                                         (list *from-tile* position)))
                                (caddr *current-game-tree*))))
           (setf *from-tile* nil)
           (princ "You may now ")
           (tag a (href (make-game-link 'pass))
             (princ "pass"))
           (princ " or make another move:"))))

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    (pick-chance-branch
     (cadr tree)
     (lazy-nth (position (apply #'max ratings) ratings)
               (caddr tree)))))

(defun get-ratings (tree player)
  (let ((board (cadr tree)))
    (labels ((number-of-dice-at (position)
               (cadr (aref board position))))
      (take-all (lazy-mapcar
                 (lambda (move)
                   (let ((path (car move)))
                     (if path
                         (let* ((source (car path))
                                (destination (cadr path))
                                (probability
                                 (aref
                                  (aref *dice-probability*
                                        (1- (number-of-dice-at destination)))
                                  (- (number-of-dice-at source) 2))))
                           (+ (* probability (rate-position (cadr move) player))
                              (* (- 1 probability) (rate-position (caddr move)
                                                                  player))))
                         (rate-position (cadr move) player))))
                 (caddr tree))))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            (lazy-nil)
            (lazy-mapcar (lambda (move)
                           (cons (car move)
                                 (mapcar (lambda (x)
                                           (limit-tree-depth x (1- depth)))
                                         (cdr move))))
                         (caddr tree)))))

(defun get-connected (board player position)
  (labels ((check-position (position visited)
             (if (and (eq (car (aref board position)) player)
                      (not (member position visited)))
                 (check-neighbors (neighbors position)
                                  (cons position visited))
                 visited))
           (check-neighbors (list visited)
             (if list
                 (check-neighbors (cdr list)
                                  (check-position (car list) visited))
                 visited)))
    check-position position '()))

(defun largest-cluster-size (board player)
  (labels ((f (position visited best)
             (if (< position *board-hex-count*)
                 (if (and (eq (car (aref board position)) player)
                          (not (member position visited)))
                     (let* ((cluster (get-connected board player position))
                            (size (length cluster)))
                       (if (> size best)
                           (f (1+ position) (append cluster visited) size)
                           (f (1+ position) (append cluster visited) best)))
                     (f (1+ position) visited best))
                 best)))
    (f 0 '() 0)))

(defun add-new-dice (board player spare-dice)
  (labels ((f (list n)
             (cond ((zerop n) list)
                   ((null list) nil)
                   (t (let ((current-player (caar list))
                            (curent-dice (cadar list)))
                        (if (and (eq current-player player)
                                 (< curent-dice *maximum-dice-per-space*))
                            (cons (list current-player (1+ curent-dice))
                                  (f (cdr list) (1- n)))
                            (cons (car list) (f (cdr list) n))))))))
    (board-array (f (coerce board 'list)
                    (largest-cluster-size board player)))))

(compile 'add-new-dice)
