(defpackage #:ecs
  (:use #:cl
        #:alexandria)
  (:export #:*ecs-manager*
           #:tick
           #:make-entity
           #:entity-id
           #:add-component
           #:remove-component
           #:component
           #:defcomponent
           #:defsys
           #:do-system))

(in-package :ecs)
