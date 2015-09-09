(in-package :ecs)

(defclass component ()
  ((name :reader name
         :initform nil)))

(defmethod print-object ((object component) stream)
  (print-unreadable-object (object stream)
    (format stream "COMPONENT: ~S" (name object))))

(defmacro defcomponent (name &body (slots))
  `(defclass ,name (component)
     ((name :initform ',name)
      ,@(loop for (slot-name slot-form) in slots
              collect (list slot-name
                            :accessor slot-name
                            :initarg (make-keyword slot-name)
                            :initform slot-form)))))

(defmacro with-cslots ((&rest slots) (component entity) &body body)
  `(let (,@(loop with c = `(component ,entity ',component)
                 for s in slots
                 when (listp s)
                 collect (list (second s) (list (first s) c))
                 unless (listp s)
                 collect (list s (list s c))))
     ,@body))

(defun make-component (name data)
  "Make a new component with the given name and data."
  (when (find-class name nil)
    (apply #'make-instance name data)))
