(in-package :cl-ecs)

(defstruct ecs
  (component-fields (make-hash-table))
  (entity-components (make-hash-table))
  (entity-attrs (make-hash-table))
  (system-meta (make-hash-table))
  (system-entities (make-hash-table)))

(defvar *ecs* nil)

(defun init-ecs ()
  "Initialize a new ECS system."
  (let ((id 0))
    (setf *ecs* (make-ecs))
    (defun new-id ()
      (incf id))
    (values)))
