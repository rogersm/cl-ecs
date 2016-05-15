(defsystem #:ecs
  :name "Entity Component System"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "An implementation of the Entity-Component-System pattern mostly used in game development."
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "ecs")))
