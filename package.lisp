(defpackage #:cl-ecs
  (:use #:cl
        #:alexandria)
  (:export #:init-ecs
           #:defcomponent
           #:add-component
           #:remove-component
           #:entity
           #:add-entity
           #:remove-entity
           #:defsys
           #:do-system
           #:cycle-system))

(in-package :cl-ecs)
