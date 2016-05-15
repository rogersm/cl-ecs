(in-package :ecs)

(defstruct ecs
  (fields (make-hash-table))
  (components (make-hash-table))
  (attrs (make-hash-table))
  (systems (make-hash-table)))

(defvar *ecs* nil)

(defun init-ecs ()
  "Initialize a new ECS system."
  (let ((id 0))
    (setf *ecs* (make-ecs))
    (defun new-id ()
      (incf id))
    (values)))

(defmacro defcomponent (name &body (fields))
  "Define a new component with the specified fields.
Also defines accessor methods for each field to be used on an entity."
  `(progn
     (setf (component-fields ',name) ',fields)
     ,@(loop :for field :in fields
             :for f = (make-keyword field)
             :collect `(defun ,field (id)
                         (entity-attr id ,f))
             :collect `(defun (setf ,field) (value id)
                         (setf (entity-attr id ,f) value)))
     ',name))

(defun all-components ()
  "Get a list of all defined components."
  (hash-table-keys (ecs-fields *ecs*)))

(defun component-fields (component)
  "Get a list of fields for the specified component."
  (gethash component (ecs-fields *ecs*)))

(defun (setf component-fields) (value component)
  "Assign a list of fields to the specified component."
  (setf (gethash component (ecs-fields *ecs*)) value))

(defun add-component (id component attrs)
  "Add a new component to the specified entity."
  (when (member component (all-components))
    (pushnew component (entity-components id)))
  (loop :for (field . value) :in (reverse (plist-alist attrs))
        :for fields = (mapcar #'make-keyword (component-fields component))
        :when (member field fields)
          :do (setf (entity-attr id field) value)))

(defun remove-component (id component)
  "Remove a component from the specified entity."
  (deletef (entity-components id) component)
  (loop :for field :in (component-fields component)
        :when (entity-attr id field)
          :do (remove-entity-attr id field)))

(defun all-entities ()
  "Get a list of all defined entities."
  (hash-table-keys (ecs-attrs *ecs*)))

(defun entity-components (id)
  "Get a list of components that the specified entity has."
  (gethash id (ecs-components *ecs*)))

(defun (setf entity-components) (components id)
  "Assign a list of components to the specified entity."
  (setf (gethash id (ecs-components *ecs*)) components))

(defun entity-attrs (id)
  "Get a list of the specified entity's attributes."
  (gethash id (ecs-attrs *ecs*)))

(defun (setf entity-attrs) (value id)
  "Assign a list of attributes to the specified entity."
  (setf (gethash id (ecs-attrs *ecs*)) value))

(defun entity-attr (id field)
  "Get the value of one of an entity's attributes."
  (getf (gethash id (ecs-attrs *ecs*)) field))

(defun (setf entity-attr) (value id field)
  "Set the value of one of an entity's attributes."
  (setf (getf (gethash id (ecs-attrs *ecs*)) field) value))

(defun remove-entity-attr (id field)
  "Remove one of an entity's attributes."
  (delete-from-plistf (gethash id (ecs-attrs *ecs*)) field))

(defun make-entity (prototype components &rest initargs)
  "Create a new entity."
  (let ((id (new-id)))
    (when prototype
      (setf (entity-components id) (copy-seq (entity-components prototype))
            (entity-attrs id) (copy-seq (entity-attrs prototype))))
    (dolist (c components)
      (add-component id c initargs))
    id))

(defun remove-entity (id)
  "Remove an entity."
  (remhash id (ecs-components *ecs*))
  (remhash id (ecs-attrs *ecs*)))

(defgeneric do-entity (system entity))

(defmacro defsys (name (&rest required) &body body)
  "Define a new system with the specified required components."
  `(progn
     (setf (required-components ',name) ',required)
     (defmethod do-entity ((system (eql ',name)) e)
       ,@body)))

(defun all-systems ()
  "Get a list of all defined systems."
  (hash-table-keys (ecs-systems *ecs*)))

(defun required-components (system)
  "Get a list of the specified system's required components."
  (gethash system (ecs-systems *ecs*)))

(defun (setf required-components) (value system)
  "Assign a list of required components to the specified system."
  (setf (gethash system (ecs-systems *ecs*)) value))

(defmethod do-system (system)
  "Execute the specified system for each entity that has all of its required
components."
  (loop :with r = (required-components system)
        :for (id . c) :in (hash-table-alist (ecs-components *ecs*))
        :when (and (intersection r c)
                   (not (set-difference r c)))
          :do (do-entity system id)))

(defun cycle-systems ()
  "Cycle through all defined systems."
  (dolist (system (all-systems))
    (do-system system)))
