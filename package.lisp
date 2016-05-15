(defpackage #:ecs
  (:use #:cl
        #:alexandria)
  (:export #:init-ecs
           #:defcomponent
           #:add-component
           #:remove-component
           #:make-entity
           #:remove-entity
           #:defsys
           #:do-system
           #:cycle-system))

(in-package :ecs)
