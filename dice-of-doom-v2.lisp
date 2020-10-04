(load "dice-of-doom-v1.lisp")
(load "lazy.lisp")

(defparameter *board-length* 4)
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
