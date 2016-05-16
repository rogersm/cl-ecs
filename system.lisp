(in-package :cl-ecs)

(defmacro defsys (name (&rest required) &body body)
  "Define a new system with the specified required components."
  `(progn
     (setf (required-components ',name) ',required)
     (defmethod do-entity ((system (eql ',name)) entity)
       (with-attrs ,required
         ,@body))))

(defgeneric do-entity (system entity))

(defun all-systems ()
  "Get a list of all defined systems."
  (hash-table-keys (ecs-system-components *ecs*)))

(defun required-components (system)
  "Get a list of the specified system's required components."
  (gethash system (ecs-system-components *ecs*)))

(defun (setf required-components) (value system)
  "Assign a list of required components to the specified system."
  (setf (gethash system (ecs-system-components *ecs*)) value))

(defun collect-system-entities (system)
  "Create a list of all of a system's entities."
  (loop :with r = (required-components system)
        :for (id . c) :in (hash-table-alist (ecs-entity-components *ecs*))
        :when (and (intersection r c)
                   (not (set-difference r c)))
          :collect id))

(defun system-entities (system)
  "Get a list of all of a system's entities."
  (gethash system (ecs-system-entities *ecs*)))

(defun (setf system-entities) (value system)
  "Assign a list of entities to the specified system."
  (setf (gethash system (ecs-system-entities *ecs*)) value))

(defun update-systems ()
  "Update the the list of entities each system processes.
Called when a component is added or removed, as well as when an entity is
removed."
  (loop :for system :in (all-systems)
        :do (setf (system-entities system) (collect-system-entities system))))

(defmethod do-system (system)
  "Execute the specified system for each entity that has all of its required
components."
  (loop :with entities = (system-entities system)
        :for id :in entities
        :do (do-entity system id)))

(defun cycle-systems ()
  "Cycle through all defined systems."
  (dolist (system (all-systems))
    (do-system system)))
