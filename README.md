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

;;; define some components.

;; define a component representing 3D coordinates.
(defcomponent coords
  (x y z))

;; define a component representing velocity.
(defcomponent velocity
  (x y z))

;;; define some systems

;; define a system that will print an entity's current coordinates.
;; within the body, you can refer to the entity with the symbol "ENTITY", and
;; individual entity attributes with prefixed with the component name, as shown
;; below.
(defsys position (coords)
  (format t "Entity ~A is at coordinates (~A, ~A, ~A)~%"
          entity
          (coords-x entity)
          (coords-y entity)
          (coords-z entity)))

;; define a system that will move an entity.
;; you can also use the WITH-ATTRS macro to save some typing, or if you do not
;; like the accessors as used in the above system definition.
(defsys move (coords velocity)
  (with-attrs (coords velocity)
    (incf coords-x velocity-x)
    (incf coords-y velocity-y)
    (incf coords-z velocity-z)))

;;; define some entities.

;; define a new entity with the 'coords' and 'velocity' components.
(make-entity nil
             '(coords velocity)
             :coords-x 10
             :coords-y 20
             :coords-z 30
             :velocity-x 1
             :velocity-y 2
             :velocity-z 3)
;; => 1

;; define a new entity using the previous as a template.
(make-entity 1
             '(coords velocity)
             :coords-x 30
             :coords-y 20
             :coords-z 10)
;; => 2

;;; execute the systems.

;; execute the 'position' system which prints out the current coordinates.
(do-system 'position)
;; => Entity 2 is at coordinates (30, 20, 10)
;; => Entity 1 is at coordinates (10, 20, 30)

;; execute the 'move' system.
(do-system 'move)
;; => NIL

;; execute the 'position' system again, to see how the 'move' system affected
;; the entities.
(do-system 'position)
;; => Entity 2 is at coordinates (31, 22, 13)
;; => Entity 1 is at coordinates (11, 22, 33)
```