(defpackage #:ecs
  (:use #:cl
        #:alexandria)
  (:export #:init-ecs
           #:defcomponent
           #:add-component
           #:remove-component
           #:entity
           #:add-entity
           #:remove-entity
           #:with-attrs
           #:defsys
           #:do-system
           #:cycle-system))

(in-package :ecs)
