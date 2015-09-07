(in-package :ecs)

(defclass ecs-system ()
  ((name :reader name)
   (required :accessor required
             :initform nil)
   (entities :accessor entities
             :initform nil)))

(defmethod print-object ((object ecs-system) stream)
  (print-unreadable-object (object stream)
    (format stream "SYSTEM: ~S" (name object))))

(defun find-system (name)
  "Find a system by name."
  (find name (systems *ecs-manager*) :key #'name))

(defmacro defsys (name &body required)
  `(progn
     (defclass ,name (ecs-system)
       ((name :initform ',name)
        (required :initform ',@required)))
     (if-let ((s (find-system ',name)))
       (setf (required s) ',@required)
       (appendf (systems *ecs-manager*) (list (make-instance ',name))))))

(defmethod do-system (system entity))
