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

;; initialize the ECS system.
(init-ecs)

;; define a component representing 3D coordinates.
(defcomponent coords
  (x y z))

;; define a component representing velocity.
(defcomponent velocity
  (vx vy vz))

;;; define some systems

;; define a system that will print an entity's current coordinates.
(defsys position (coords)
  (format t "Entity ~A is at coordinates (~A, ~A, ~A)~%"
          e (x e) (y e) (z e)))

;; define a system that will move an entity.
(defsys move (coords velocity)
  (incf (x e) (vx e))
  (incf (y e) (vy e))
  (incf (z e) (vz e)))

;; define a new entity with the 'coords' and 'velocity' components.
(make-entity nil '(coords velocity) :x 10 :y 20 :z 30 :vx 1 :vy 2 :vz 3)

;; define a new entity using the previous as a template.
(make-entity 1 '(coords velocity) :x 30 :y 20 :z 10)

;; execute the 'position' system which prints out the current coordinates.
(do-system 'position)
;; => Entity 2 is at coordinates (30, 20, 10)
;; => Entity 1 is at coordinates (10, 20, 30)

;; execute the 'move' system.
(do-system 'move)

;; execute the 'position' system again, to see how the 'move' system affected
;; the entities.
(do-system 'position)
;; => Entity 2 is at coordinates (31, 22, 13)
;; => Entity 1 is at coordinates (11, 22, 33)
```