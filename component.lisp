(in-package :ecs)

(defclass component ()
  ((name :reader name
         :initform nil)))

(defmethod print-object ((object component) stream)
  (print-unreadable-object (object stream)
    (format stream "COMPONENT: ~S" (name object))))

(defmacro defcomponent (name &body (slots))
  `(defclass ,name (component)
     ,(loop for (slot-name slot-form) in slots
            collect `(name :initform ',name)
            collect (list slot-name
                          :accessor slot-name
                          :initarg (make-keyword slot-name)
                          :initform slot-form))))

(defun make-component (name data)
  "Make a new component with the given name and data."
  (when (find-class name nil)
    (apply #'make-instance name data)))
