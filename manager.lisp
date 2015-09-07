(in-package :ecs)

(defclass ecs-manager ()
  ((entity-count :accessor entity-count
                 :initform 0)
   (systems :accessor systems
            :initform nil)))

(defvar *ecs-manager* (make-instance 'ecs-manager))

(defun tick ()
  "Used to step through all systems each iteration of the game loop."
  (dolist (s (systems *ecs-manager*))
    (dolist (e (entities s))
      (do-system s e))))
