(load "dice-of-doom-v1.lisp")
(load "lazy.lisp")

(defparameter *board-length* 5)
(defparameter *board-hex-count* (* *board-length* *board-length*))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
                       (game-tree (add-new-dice board player
                                                (1- spare-dice))
                                  (mod (1+ player) *number-of-players*)
                                  0
                                  t))
                 moves)))

(defun attacking-moves (board current-player spare-dice)
  (labels ((player (position)
             (car (aref board position)))
           (dice (position)
             (cadr (aref board position))))
    (lazy-mapcan
     (lambda (source)
       (if (eq (player source) current-player)
         (lazy-mapcan
          (lambda (destination)
            (if (and (not (eq (player destination)
                                current-player))
                     (> (dice source) (dice destination)))
                (make-lazy
                 (list (list (list source destination)
                             (game-tree (board-attack board
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

(defun handle-human (tree)
  (fresh-line)
  (princ "Choose your move: ")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                       (format t "~a -> ~a" (car action) (cadr action))
                       (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-versus-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-versus-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            (lazy-nil)
            (lazy-mapcar (lambda (move)
                           (list (car move)
                                 (limit-tree-depth (cadr move) (1- depth))))
                         (caddr tree)))))

(defparameter *ai-level* 4)

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
                              (car tree))))
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
                    (caddr tree)))))

(defun play-versus-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-versus-computer (handle-human tree)))
        (t (play-versus-computer (handle-computer tree)))))

(defun score-board (board player)
  (loop for hex across board
     for position from 0
     sum (if (eq (car hex) player)
             (if (threatened position board)
                 1
                 2)
             -1)))

(defun threatened (position board)
  (let* ((hex (aref board position))
         (player (car hex))
         (dice (cadr hex)))
    (loop for neighbor in (neighbors position)
       do (let* ((neighboring-hex (aref board neighbor))
                 (neighboring-player (car neighboring-hex))
                 (neighboring-dice (cadr neighboring-hex)))
            (when (and (not (eq player neighboring-player))
                       (> neighboring-dice dice))
              (return t))))))

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (score-board (cadr tree) player))))

(defun a-b-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
             (unless (lazy-null moves)
               (let ((x (a-b-rate-position (cadr (lazy-car moves))
                                           player
                                           upper-limit
                                           lower-limit)))
                 (if (>= x upper-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))

(defun a-b-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
             (unless (lazy-null moves)
               (let ((x (a-b-rate-position (cadr (lazy-car moves))
                                           player
                                           upper-limit
                                           lower-limit)))
                 (if (<= x lower-limit)
                     (list x)
                     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))

(defun a-b-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
        (if (eq (car tree) player)
            (apply #'max (a-b-get-ratings-max tree
                                              player
                                              upper-limit
                                              lower-limit))
            (apply #'min (a-b-get-ratings-min tree
                                              player
                                              upper-limit
                                              lower-limit)))
        (score-board (cadr tree) player))))

(defun handle-computer (tree)
  (let ((ratings (a-b-get-ratings-max (limit-tree-depth tree *ai-level*)
                                      (car tree)
                                      most-positive-fixnum
                                      most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))

;; (play-versus-computer (game-tree (generate-board) 0 0 t))
