(in-package :cl-ecs)

(defmacro defsys (name (required group) &body body)
  "Define a new system."
  (let ((entities (gensym)))
    `(progn
       (setf (required-components ',name) ',required
             (system-group ',name) ',group)
       (defmethod %do-entities ((system (eql ',name)) &rest ,entities)
         (destructuring-bind ,group ,entities
           ,@body)))))

(defgeneric %do-entities (system &rest entities))

(defun all-systems ()
  "Get a list of all defined systems."
  (hash-table-keys (ecs-system-meta *ecs*)))

(defun required-components (system)
  "Get a list of the specified system's required components."
  (getf (gethash system (ecs-system-meta *ecs*)) :required))

(defun (setf required-components) (value system)
  "Assign a list of required components to the specified system."
  (setf (getf (gethash system (ecs-system-meta *ecs*)) :required) value))

(defun system-group (system)
  "Get the list of grouping information for the specified system."
  (getf (gethash system (ecs-system-meta *ecs*)) :group))

(defun (setf system-group) (value system)
  "Assign the list of grouping information to the specified system."
  (setf (getf (gethash system (ecs-system-meta *ecs*)) :group) value))

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
  "Execute the specified system. The system definition's grouping determines
parallel processing of entities."
  (let ((group (length (system-group system)))
        (entities (system-entities system)))
    (when (>= (length entities) group)
      (if (= group 1)
          (dolist (e entities)
            (%do-entities system e))
          (map-combinations
           (lambda (x) (apply #'%do-entities system x)) entities :length group)))))

(defun cycle-systems ()
  "Cycle through all defined systems."
  (dolist (system (all-systems))
    (do-system system)))
