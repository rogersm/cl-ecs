(in-package :ecs)

(defclass ecs-manager ()
  ((entity-count :accessor entity-count
                 :initform 0)
   (entities :accessor entities
             :initform (make-hash-table))
   (systems :accessor systems
            :initform nil)))

(defvar *ecs-manager* (make-instance 'ecs-manager))

(defun tick ()
  "Used to step through all systems each iteration of the game loop."
  (dolist (s (systems *ecs-manager*))
    (dolist (id (entities s))
      (do-system s (find-entity id)))))
