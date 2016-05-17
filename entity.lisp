(in-package :cl-ecs)

(defstruct (entity (:conc-name nil))
  components
  attributes)

(defmacro add-entity (prototype &body components)
  "A helper macro to create an entity."
  (let* ((parts
           (mapcar
            (lambda (component)
              (loop :for form :in (cdr component)
                    :for i :from 0
                    :when (evenp i)
                      :collect form :into slots
                      :and
                        :collect (gensym (symbol-name form)) :into vars
                    :when (oddp i)
                      :collect form :into values
                    :finally (return
                               (list
                                (car component)
                                (mapcar
                                 (lambda (x)
                                   (make-keyword
                                    (format nil "~A-~A" (car component) x)))
                                 slots)
                                (mapcar #'list vars values)))))
            components))
         (bindings
           (loop :for (nil nil binds) :in parts
                 :append (loop :for b :in binds :collect b))))
    `(let (,@bindings)
       (%add-entity
        ',prototype
        (list
         ,@(mapcan (lambda (part)
                     (list
                      `(list ',(first part)
                             ,@(mapcan (lambda (sym bind)
                                         `(,sym ,(first bind)))
                                       (second part) (third part)))))
                   parts))))))

(defun all-entities ()
  "Get a list of all defined entities."
  (hash-table-keys (ecs-entities *ecs*)))

(defun entity-components (id)
  "Get a list of all components of the specified entity."
  (components (gethash id (ecs-entities *ecs*))))

(defun (setf entity-components) (value id)
  "Assign a list of components to the specified entity."
  (setf (components (gethash id (ecs-entities *ecs*))) value))

(defun entity-attrs (id)
  "Get a list of the specified entity's attributes."
  (attributes (gethash id (ecs-entities *ecs*))))

(defun (setf entity-attrs) (value id)
  "Assign a list of attributes to the specified entity."
  (setf (attributes (gethash id (ecs-entities *ecs*))) value))

(defun entity-attr (id field)
  "Get the value of one of an entity's attributes."
  (getf (entity-attrs id) field))

(defun (setf entity-attr) (value id field)
  "Set the value of one of an entity's attributes."
  (setf (getf (entity-attrs id) field) value))

(defun remove-entity-attr (id field)
  "Remove one of an entity's attributes."
  (delete-from-plistf (entity-attrs id) field))

(defun %add-entity (prototype components)
  "Internal function for creating a new entity."
  (let ((id (new-id)))
    (setf (gethash id (ecs-entities *ecs*)) (make-entity))
    (when prototype
      (setf (entity-components id) (copy-seq (entity-components prototype))
            (entity-attrs id) (copy-seq (entity-attrs prototype))))
    (loop :for (name . attrs) :in components
          :do (add-component id name attrs))
    id))

(defun remove-entity (id)
  "Remove an entity."
  (remhash id (ecs-entities *ecs*))
  (update-systems))
