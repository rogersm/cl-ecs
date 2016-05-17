# ECS

This is an implementation of the Entity-Component-System pattern, mostly
used for game development.

## Entities

Entities are simply containers of data. They contain no information other than
a unique ID and a list of attributes.

`add-entity` is used to create a new unique entity, which can have components
attached to it during or after creation.

```lisp
(add-entity nil
  (component1 :attr1 10 :attr2 (list 1 2 3) ...)
  (component2 ...)
```

## Components

Components are nothing more than a list of 'fields'. Data is not stored directly
in a component, but entities possessing a particular component inherits that
component's fields as 'attributes' that can be modified for the specific entity.

You can define a new component with `defcomponent`.

```lisp
(defcomponent component-name (field1 field2 field3 ...)
```

You can use `add-component` and `remove-component` on an entity to define
properties or behavior. You can add components when you create an entity, or
on-the-fly later in the entity's life. The result will be an immediate change in
behavior, depending on the systems you have defined.

## Systems

A system iterates through all entities that have the particular system's
required components, performing some actions.

Unique to this implementation, systems can run on entities in parallel by making
use of combinations. That is, a system definition can specify that it is to run
on 2 or more entities at once instead of just 1. In the case of combination
systems, every possible combination will be executed, having access to each
entity of the combination grouping in the body of the system. This can be very
useful for collision detection and other systems.

You can define a new system with `defsys`.

```lisp
;; an example of a combination system with a grouping of 2.
;; every combination of 2 entities meeting the system's required components
;; will be acted upon in parallel.
;; both entity1 and entity2 are accessible in the body.
(defsys system1 ((component1 component2) (entity1 entity2))
  (do-something))
```

## Example

Below is a very basic usage example:

```lisp
(in-package :ecs)

;; Initialize the ECS system.

(init-ecs)

;;; Define some components.

(defcomponent coords
  (x y z))

(defcomponent velocity
  (x y z))

;;; Define some systems.

(defsys position ((coords) (e1 e2))
  (dolist (e (list e1 e2))
    (format t "Entity ~A at (~A, ~A, ~A)~%"
            e (coords-x e) (coords-y e) (coords-z e)))
  (format t "Entity ~A is (~A, ~A, ~A) away from Entity ~A~%"
          e1
          (- (coords-x e1) (coords-x e2))
          (- (coords-y e1) (coords-y e2))
          (- (coords-z e1) (coords-z e2))
          e2))

(defsys move ((coords velocity) (e1))
  (incf (coords-x e1) (velocity-x e1))
  (incf (coords-y e1) (velocity-y e1))
  (incf (coords-z e1) (velocity-z e1)))

;;; Define some entities.

(add-entity nil
  (coords :x 10 :y 20 :z 30)
  (velocity :x 1 :y 1 :z 1))
;; => 1

;; The first argument, if non-NIL, refers to the ID of an existing entity to be
;; used as a prototype. In the following entity, we save some typing and
;; inherit the velocity attributes of our first entity above.

(add-entity 1
  (velocity :x 4 :y 4 :z 4))
;; => 2

(add-entity 2
  (velocity :x 20 :y 5 :z 19))
;; => 3

;;; Execute the systems.

;; Execute the 'position' system to see how all combinations are processed in
;; parallel.
(do-system 'position)
;; =>
;; Entity 3 at (10, 20, 30)
;; Entity 2 at (10, 20, 30)
;; Entity 3 is (0, 0, 0) away from Entity 2
;; Entity 3 at (10, 20, 30)
;; Entity 1 at (10, 20, 30)
;; Entity 3 is (0, 0, 0) away from Entity 1
;; Entity 2 at (10, 20, 30)
;; Entity 1 at (10, 20, 30)
;; Entity 2 is (0, 0, 0) away from Entity 1

;; You can also run each system once automatically with CYCLE-SYSTEMS.
(cycle-systems)
;; =>
;; Entity 3 at (30, 25, 49)
;; Entity 2 at (14, 24, 34)
;; Entity 3 is (16, 1, 15) away from Entity 2
;; Entity 3 at (30, 25, 49)
;; Entity 1 at (11, 21, 31)
;; Entity 3 is (19, 4, 18) away from Entity 1
;; Entity 2 at (14, 24, 34)
;; Entity 1 at (11, 21, 31)
;; Entity 2 is (3, 3, 3) away from Entity 1
```