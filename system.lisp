(in-package :cl-ecs)

(defstruct (system (:conc-name nil))
  required
  grouping
  entities)

(defmacro defsys (name (required grouping) &body body)
  "Define a new system."
  (let ((entities (gensym)))
    `(progn
       (setf (gethash ',name (ecs-systems *ecs*))
             (make-system :required ',required
                          :grouping ',grouping))
       (cache-system-entities)
       (defmethod %do-entities ((system (eql ',name)) &rest ,entities)
         (block ,name
           (destructuring-bind ,grouping ,entities
             ,@body))))))

(defgeneric %do-entities (system &rest entities))

(defun all-systems ()
  "Get a list of all defined systems."
  (hash-table-keys (ecs-systems *ecs*)))

(defun required-components (system)
  "Get a list of the specified system's required components."
  (required (gethash system (ecs-systems *ecs*))))

(defun (setf required-components) (value system)
  "Assign a list of required components to the specified system."
  (setf (required (gethash system (ecs-systems *ecs*))) value))

(defun system-grouping (system)
  "Get the list of grouping information for the specified system."
  (grouping (gethash system (ecs-systems *ecs*))))

(defun collect-system-entities (system)
  "Create a list of all of a system's entities."
  (loop :with r = (required-components system)
        :for (id . e) :in (hash-table-alist (ecs-entities *ecs*))
        :for c = (components e)
        :when (or (not r)
                  (and (listp r) (all r c))
                  (and (eq r :none) (not c))
                  (and (eq r :any) c))
          :collect id))

(defun system-entities (system)
  "Get a list of all of a system's entities."
  (entities (gethash system (ecs-systems *ecs*))))

(defun (setf system-entities) (value system)
  "Assign a list of entities to the specified system."
  (setf (entities (gethash system (ecs-systems *ecs*))) value))

(defun cache-system-entities ()
  "Update the the list of entities for all systems."
  (loop :for system :in (all-systems)
        :do (setf (system-entities system) (collect-system-entities system))))

(defmethod do-system (system)
  "Execute the specified system. The system definition's grouping determines
parallel processing of entities."
  (let ((grouping (length (system-grouping system)))
        (entities (system-entities system))
        (result))
    (when (>= (length entities) grouping)
      (if (= (length entities) 1)
          (setf result (apply #'%do-entities system entities))
          (map-combinations
           (lambda (x) (setf result (apply #'%do-entities system x)))
           entities :length grouping))
      result)))

(defun cycle-systems ()
  "Cycle through all defined systems."
  (dolist (system (all-systems))
    (do-system system)))
