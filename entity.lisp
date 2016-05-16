(in-package :cl-ecs)

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
                                `(intern (symbol-name ',(car component)))
                                (mapcar
                                 (lambda (x)
                                   (make-keyword
                                    (format nil "~A-~A" (car component) x)))
                                 slots)
                                (mapcar #'list vars values)))))
            components))
         (bindings
           (loop :for (nil nil binds) :in parts
                 :append (loop :for b :in binds :collect b) :into bindings
                 :finally (return (nreverse bindings)))))
    `(let (,@bindings)
       (%add-entity
        ',prototype
        (list
         ,@(mapcan (lambda (part)
                     (list (first part)
                           `(list ,@(mapcan (lambda (sym bind)
                                              `(,sym ,(first bind)))
                                            (second part) (third part)))))
                   parts))))))

(defmacro with-attrs (components &body body)
  "A helper macro to access an entity's attributes within a system definition."
  `(symbol-macrolet
       (,@(loop :for c :in components
                :append (loop :for f :in (component-fields c)
                              :collect (list f `(,f entity)))))
     ,@body))

(defun all-entities ()
  "Get a list of all defined entities."
  (hash-table-keys (ecs-entity-attrs *ecs*)))

(defun entity-components (id)
  "Get a list of all components of the specified entity."
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

(defun remove-entity-attr (id field)
  "Remove one of an entity's attributes."
  (delete-from-plistf (gethash id (ecs-entity-attrs *ecs*)) field))

(defun %add-entity (prototype components)
  "Internal function for creating a new entity."
  (let ((id (new-id)))
    (when prototype
      (setf (entity-components id) (copy-seq (entity-components prototype))
            (entity-attrs id) (copy-seq (entity-attrs prototype))))
    (loop :for (name . attrs) :in (plist-alist components)
          :do (add-component id name attrs))
    id))

(defun remove-entity (id)
  "Remove an entity."
  (remhash id (ecs-entity-components *ecs*))
  (remhash id (ecs-entity-attrs *ecs*))
  (update-systems))
