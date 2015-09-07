(in-package :ecs)

(defclass entity ()
  ((id :reader entity-id
       :initform (incf (entity-count *ecs-manager*)))
   (components :accessor components
               :initform nil)))

(defmethod print-object ((object entity) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~{~S~^, ~})" (mapcar #'name (components object)))))

(defun make-entity (&key components)
  "Create a new entity without any components."
  (let ((entity (make-instance 'entity)))
    (dolist (name components)
      (add-component entity name))
    entity))

(defmacro make-batch ((&rest items) (&rest components) &body body)
  "Macro for creating multiple entities with the same components."
  `(let (,@(loop for i in items
                 for e = `(make-entity :components ',components)
                 collect (list i e)))
     ,@body))

(defun add-component (entity name &rest args)
  "Add a component by name to an entity and update all systems."
  (when-let ((component (make-component name args)))
    (pushnew component (components entity) :key #'name)
    (update-system entity)))

(defun remove-component (entity name)
  "Remove a component by name from an entity and update all systems."
  (deletef (components entity) name :key #'name)
  (update-system entity))

(defun component (entity name)
  "Find a component of an entity given its name."
  (find name (components entity) :key #'name))

(defun component-names (entity)
  "Return a list of component names for an entity."
  (mapcar #'name (components entity)))

(defun update-system (entity)
  "Register or de-register an entity with all applicable systems.
   Called when a component is added or removed from an entity."
  (dolist (s (systems *ecs-manager*))
    (if (subsetp (required s) (component-names entity))
      (pushnew entity (entities s))
      (deletef (entities s) entity))))
