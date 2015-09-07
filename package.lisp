(defpackage #:ecs
  (:use #:cl
        #:alexandria)
  (:export #:*ecs-manager*
           #:tick
           #:make-entity
           #:add-component
           #:remove-component
           #:component
           #:defcomponent
           #:defsys
           #:do-system))

(in-package :ecs)
