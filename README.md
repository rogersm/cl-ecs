# ECS

This is a basic implementation of the Entity-Component-System pattern, mostly
used for game development. It has not been optimized, and was created in a
single day to help myself learn more about it. Pull requests are always welcome.

## Entities

Entities are simply containers of data. They contain no information other than
a unique ID and a list of data (components).

`make-entity` is used to create a new unique entity, which can have components
attached to it during or after creation.

## Components

Components are pure data that can be attached to entities.

You can define a new component with `defcomponent`.

You can use `add-component` and `remove-component` on an entity to define
properties or behavior.

You can add components when you create an entity, or on-the-fly later in the
entity's life. The result will be an immediate change in behavior, depending
on the systems you have defined.

## Systems

A system iterates through all entities that have the particular system's
required components, performing some actions.

You can define a new system with `defsys`.

## Example

Below is a very basic usage example:

```lisp
(in-package :ecs)

;; Initialize the ECS system.
(init-ecs)

;;; Define some components.

;; Define a component representing 3D coordinates.
(defcomponent coords
  (x y z))

;; Define a component representing velocity.
(defcomponent velocity
  (x y z))

;;; Define some systems.

;; Define a system that will print an entity's current coordinates.
;; Within the body, you can refer to the entity with the symbol 'ENTITY', and
;; individual entity attributes prefixed with the component name, as shown
;; below.
(defsys position (coords)
  (format t "Entity ~A is at coordinates (~A, ~A, ~A)~%"
          entity coords-x coords-y coords-z))

;; Define a system that will move an entity.
(defsys move (coords velocity)
  (incf coords-x velocity-x)
  (incf coords-y velocity-y)
  (incf coords-z velocity-z))

;;; Define some entities.

;; Define a new entity with the 'COORDS' and 'VELOCITY' components.
(add-entity nil
  (coords :x 10 :y 20 :z 30)
  (velocity :x 1 :y 2 :z 3))
;; => 1

;; Define a new entity using the previous as a template.
(add-entity 1
  (coords :x 30 :y 20 :z 10))
;; => 2

;;; Execute the systems.

;; Execute the 'POSITION' system which prints out the current coordinates.
(do-system 'position)
;; => Entity 2 is at coordinates (30, 20, 10)
;; => Entity 1 is at coordinates (10, 20, 30)

;; Execute the 'MOVE' system.
(do-system 'move)
;; => NIL

;; Execute the 'position' system again, to see how the 'MOVE' system affected
;; the entities.
(do-system 'position)
;; => Entity 2 is at coordinates (31, 22, 13)
;; => Entity 1 is at coordinates (11, 22, 33)

;; You can also run each system once automatically with CYCLE-SYSTEMS.
(cycle-systems)
;; => Entity 2 is at position (32, 24, 16)
;; => Entity 1 is at position (12, 24, 36)
```