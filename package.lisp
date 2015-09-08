(defpackage #:ecs
  (:use #:cl
        #:alexandria)
  (:export #:*ecs-manager*
           #:tick
           #:make-entity
           #:make-batch
           #:entity-id
           #:entity-by-id
           #:entity-by-tag
           #:add-component
           #:remove-component
           #:component
           #:defcomponent
           #:defsys
           #:do-system))

(in-package :ecs)
