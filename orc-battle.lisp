(defparameter *player-health* nil)
(defparameter *player-agility* 7)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (monster)
           (or (monster-dead monster) (monster-attack monster)))
         *monsters*)
    (game-loop)))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (case (read)
    (s (monster-hit (pick-monster)
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((power (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ")
         (princ power)
         (fresh-line)
         (monster-hit (pick-monster) power)
         (unless (monsters-dead)
           (monster-hit (pick-monster) power))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
                 (unless (monsters-dead)
                   (monster-hit (random-monster) 1))))))

(defun randval (ceiling)
  (1+ (random (max 1 ceiling))))

(defun random-monster ()
  (let ((monster (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead monster)
        (random-monster)
        monster)))

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #: ")
  (let ((monster-number (read)))
    (if (not (and (integerp monster-number)
                  (>= monster-number 1)
                  (<= monster-number *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
        (let ((monster (aref *monsters* (1- monster-number))))
          (if (monster-dead monster)
              (progn (princ "That monster is already dead.")
                     (pick-monster))
              monster)))))

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (unused)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (monster)
  (<= (monster-health monster) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes: ")
  (let ((monsters-count 0))
    (map 'list
         (lambda (monster)
           (fresh-line)
           (princ "   ")
           (princ (incf monsters-count))
           (princ ". ")
           (if (monster-dead monster)
               (princ "**DEAD**")
               (progn (princ "(Health=")
                      (princ (monster-health monster))
                      (princ ") ")
                      (monster-show monster))))
         *monsters*)))

(defstruct monster (health (randval 10)))

(defmethod monster-hit (monster damage)
  (decf (monster-health monster) damage)
  (if (monster-dead monster)
      (progn (princ "You killed the ")
             (princ (type-of monster))
             (princ "! "))
      (progn (princ "You hit the ")
             (princ (type-of monster))
             (princ ", knocking off ")
             (princ damage)
             (princ " health points! "))))

(defmethod monster-show (monster)
  (princ "A fierce ")
  (princ (type-of monster)))

(defmethod monster-attack (monster))

(defstruct (orc (:include monster))
  (club-level (randval 8)))

(push #'make-orc *monster-builders*)

(defmethod monster-show ((monster orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level monster))
  (princ " club"))

(defmethod monster-attack ((monster orc))
  (let ((damage (randval (orc-club-level monster))))
    (princ "An orc swings his club at you and knocks off ")
    (princ damage)
    (princ " of your health points. ")
    (decf *player-health* damage)))

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((monster hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health monster))
  (princ " heads."))

(defmethod monster-hit ((monster hydra) damage)
  (decf (monster-health monster) damage)
  (if (monster-dead monster)
      (progn (princ "The corpse of the fully decapitated and decapacitated hydra")
             (princ " falls to the floor!"))
      (progn (princ "You lop off ")
             (princ damage)
             (princ " of the hydra's heads! "))))

(defmethod monster-attack ((monster hydra))
  (let ((damage (randval (ash (monster-health monster) -1))))
    (princ "A hydra attacks you with ")
    (princ damage)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health monster))
    (decf *player-health* damage)))

(defstruct (slime-mold (:include monster))
  (sliminess (randval 5)))

(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((monster slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess monster)))

(defmethod monster-attack ((monster slime-mold))
  (let ((damage (randval (slime-mold-sliminess monster))))
    (princ "A slime mold wraps around your legs and decreases your agility by ")
    (princ damage)
    (princ "! ")
    (decf *player-agility* damage)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*))))

(defstruct (brigand (:include monster)))

(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((monster brigand))
  (let ((attribute (max *player-health* *player-agility* *player-strength*)))
    (cond
      ((= attribute *player-health*)
       (princ "A brigand hits you with his slingshot,")
       (princ " taking off 2 health points! ")
       (decf *player-health* 2))
      ((= attribute *player-agility*)
       (princ "A brigand catches your leg with his whip,")
       (princ " taking off 2 agility points! ")
       (decf *player-agility* 2))
      ((= attribute *player-strength*)
       (princ "A brigand cuts your arm with his whip,")
       (princ " taking off 2 strength points! ")
       (decf *player-strength* 2)))))
