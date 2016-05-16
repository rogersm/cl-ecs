(in-package :cl-ecs)

(defstruct ecs
  (component-fields (make-hash-table))
  (entity-components (make-hash-table))
  (entity-attrs (make-hash-table))
  (system-components (make-hash-table))
  (system-entities (make-hash-table)))

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
Also defines accessors for each field to be used on an entity."
  `(progn
     ,@(loop :for f :in fields
             :for field = (intern (format nil "~A-~A" name f))
             :for key = (make-keyword field)
             :do (add-component-field name field)
             :collect `(defun ,field (id)
                         (entity-attr id ,key))
             :collect `(defun (setf ,field) (value id)
                         (setf (entity-attr id ,key) value)))
     ',name))

(defun all-components ()
  "Get a list of all defined components."
  (hash-table-keys (ecs-component-fields *ecs*)))

(defun component-fields (component)
  "Get a list of fields for the specified component."
  (gethash component (ecs-component-fields *ecs*)))

(defun (setf component-fields) (value component)
  "Assign a list of fields to the specified component."
  (setf (gethash component (ecs-component-fields *ecs*)) value))

(defun add-component-field (component field)
  "Add a new field to the specified component."
  (pushnew field (component-fields component)))

(defun add-component (id component attrs)
  "Add a new component to the specified entity."
  (when (member component (all-components))
    (pushnew component (entity-components id)))
  (update-systems)
  (loop :for (field . value) :in (reverse (plist-alist attrs))
        :for fields = (mapcar #'make-keyword (component-fields component))
        :when (member field fields)
          :do (setf (entity-attr id field) value)))

(defun remove-component (id component)
  "Remove a component from the specified entity."
  (deletef (entity-components id) component)
  (update-systems)
  (loop :for field :in (component-fields component)
        :when (entity-attr id field)
          :do (remove-entity-attr id field)))

(defun all-entities ()
  "Get a list of all defined entities."
  (hash-table-keys (ecs-entity-attrs *ecs*)))

(defun entity-components (id)
  "Get a list of components that the specified entity has."
  (gethash id (ecs-entity-components *ecs*)))

(defun (setf entity-components) (components id)
  "Assign a list of components to the specified entity."
  (setf (gethash id (ecs-entity-components *ecs*)) components))

(defun entity-attrs (id)
  "Get a list of the specified entity's attributes."
  (gethash id (ecs-entity-attrs *ecs*)))

(defun (setf entity-attrs) (value id)
  "Assign a list of attributes to the specified entity."
  (setf (gethash id (ecs-entity-attrs *ecs*)) value))

(defun entity-attr (id field)
  "Get the value of one of an entity's attributes."
  (getf (gethash id (ecs-entity-attrs *ecs*)) field))

(defun (setf entity-attr) (value id field)
  "Set the value of one of an entity's attributes."
  (setf (getf (gethash id (ecs-entity-attrs *ecs*)) field) value))

(defmacro with-attrs (components &body body)
  "A helper macro to access an entity's attributes within a system definition."
  `(symbol-macrolet
       (,@(loop :for c :in components
                :append (loop :for f :in (component-fields c)
                              :collect (list f `(,f entity)))))
     ,@body))

(defun remove-entity-attr (id field)
  "Remove one of an entity's attributes."
  (delete-from-plistf (gethash id (ecs-entity-attrs *ecs*)) field))

(defun add-entity (prototype components &rest initargs)
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
  (remhash id (ecs-entity-components *ecs*))
  (remhash id (ecs-entity-attrs *ecs*))
  (update-systems))

(defgeneric do-entity (system entity))

(defmacro defsys (name (&rest required) &body body)
  "Define a new system with the specified required components."
  `(progn
     (setf (required-components ',name) ',required)
     (defmethod do-entity ((system (eql ',name)) entity)
       ,@body)))

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
