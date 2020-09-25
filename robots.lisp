(defparameter *board-width* 64)
(defparameter *board-height* 16)
(defparameter *board-size* (* *board-width* *board-height*))

(defparameter *robot-count* 10)

(defun robots ()
  (loop named main
     with directions = '((q . -65) (w . -64) (e . -63)
                         (a .  -1)           (d .   1)
                         (z .  63) (x .  64) (c .  65))
     for position = 544
     then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave: ")
                 (force-output)
                 (let* ((character (read))
                        (direction (assoc character directions)))
                   (cond (direction (+ position (cdr direction)))
                         ((eq 't character) (random *board-size*))
                         ((eq 'l character) (return-from main 'bye))
                         (t position))))
     for robots = (loop repeat *robot-count*
                     collect (random *board-size*))
     then (loop for robot-position in robots
             collect (if (> (count robot-position robots) 1)
                         robot-position
                         (cdar (sort (loop for (k . d) in directions
                                        for new-robot-position = (+ robot-position d)
                                        collect (cons (+ (abs (- (mod new-robot-position 64)
                                                                 (mod position 64)))
                                                         (abs (- (ash new-robot-position -6)
                                                                 (ash position -6))))
                                                      new-robot-position))
                                     '<
                                     :key #'car))))
     when (loop for robot-position in robots
             always (> (count robot-position robots) 1))
     return 'player-wins
     do (format t
                "~%|~{~<|~%|~,65:;~A~>~}|"
                (loop for location
                   below *board-size*
                   collect (cond ((member location robots)
                                  (cond ((= location position) (return-from main 'player-loses))
                                        ((> (count location robots) 1) #\#)
                                        (t #\A)))
                                 ((= location position) #\@)
                                 (t #\ ))))))
