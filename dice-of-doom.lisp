(defparameter *number-of-players* 2)
(defparameter *maximum-dice-per-space* 3)
(defparameter *board-length* 3)
(defparameter *board-hex-count* (* *board-length* *board-length*))

(defun board-array (list)
  (make-array *board-hex-count* :initial-contents list))

(defun generate-board ()
  (board-array (loop for n below *board-hex-count*
                  collect (list (random *number-of-players*)
                                (1+ (random *maximum-dice-per-space*))))))

(defun player-letter (number)
  (code-char (+ 97 number)))

(defun draw-board (board)
  (loop for y-position below *board-length*
     do (progn (fresh-line)
               (loop repeat (- *board-length* y-position)
                  do (princ "  "))
               (loop for x-position below *board-length*
                  for hex = (aref board (+ x-position
                                           (* *board-length* y-position)))
                  do (format t "~a-~a " (player-letter (first hex))
                             (second hex))))))

(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *number-of-players*)
                             0
                             t))
            moves)))

(defun attacking-moves (board current-player spare-dice)
  (labels ((player (position)
             (car (aref board position)))
           (dice (position)
             (cadr (aref board position))))
    (mapcan (lambda (source)
              (when (eq (player source) current-player)
                (mapcan (lambda (destination)
                          (when (and (not (eq (player destination) current-player))
                                     (> (dice source) (dice destination)))
                            (list
                             (list
                              (list source destination)
                              (game-tree
                               (board-attack board current-player source destination
                                             (dice source))
                               current-player
                               (+ spare-dice (dice destination))
                               nil)))))
                        (neighbors source))))
            (loop for n below *board-hex-count*
                 collect n))))

(defun neighbors (position)
  (let ((up (- position *board-length*))
        (down (+ position *board-length*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod position *board-length*))
                             (list (1- up) (1- position)))
                           (unless (zerop (mod (1+ position) *board-length*))
                             (list (1+ position) (1+ down))))
       when (and (>= p 0) (< p *board-hex-count*))
         collect p)))

(defun board-attack (board player source destination dice)
  (board-array (loop for position from 0
                  for hex across board
                  collect (cond ((eq position source) (list player 1))
                                ((eq position destination) (list player (1- dice)))
                                (t hex)))))

(defun add-new-dice (board player spare-dice)
  (labels ((f (board-subset dice-available)
             (cond ((null board-subset) nil)
                   ((zerop dice-available) board-subset)
                   (t (let ((current-player (caar board-subset))
                            (current-dice (cadar board-subset)))
                        (if (and (eq current-player player)
                                 (< current-dice *maximum-dice-per-space*))
                            (cons (list current-player (1+ current-dice))
                                  (f (cdr board-subset) (1- dice-available)))
                            (cons (car board-subset)
                                  (f (cdr board-subset) dice-available))))))))
    (board-array (f (coerce board 'list) spare-dice))))

(defun play-versus-human (tree)
  (print-info tree)
  (if (caddr tree)
      (play-versus-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "Choose your move: ")
  (let ((moves (caddr tree)))
    (loop for move in moves
       for n from 1
       do (let ((action (car move)))
            (fresh-line)
            (format t "~a. " n)
            (if action
                (format t "~a -> ~a" (car action) (cadr action))
                (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

(defun winners (board)
  (let* ((tally (loop for hex across board
                   collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if moves
        (apply (if (eq (car tree) player)
                   #'max
                   #'min)
               (get-ratings tree player))
        (let ((w (winners (cadr tree))))
          (if (member player w)
              (/ 1 (length w))
              0)))))

(defun get-ratings (tree player)
  (mapcar (lambda (move)
            (rate-position (cadr move) player))
          (caddr tree)))

(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defun play-versus-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-versus-computer (handle-human tree)))
        (t (play-versus-computer (handle-computer tree)))))
