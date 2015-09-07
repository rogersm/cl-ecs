# ECS

This is a basic implementation of the Entity-Component-System pattern, mostly used for game
development. It has not been optimized, and was created in a single day to help myself learn more
about it. Pull requests are always welcome.

## Entities

Entities are simply containers of data. They contain no information other than a unique ID and a list of
data (components).

`make-entity` is used to create a new unique entity, which can later have behavior attached to it in
the form of components.

## Components

Components are pure data that can be attached to entities.

You can define a new component with `defcomponent`.

You can use `add-component` and `remove-component` on an entity to define properties or behavior.

You can do this when you first create an entity, or on-the-fly during the program's loop -- the
result will be an immediate change in behavior, depending on the systems you have defined.

## Systems

A system iterates through all entities that have the particular system's required components,
performing some actions.

You can define a new system with `defsys`.

## Example

Below is a very basic usage example:

```lisp
(in-package :ecs)

;; define a component representing the total health of a game entity.
(defcomponent health
  ((value 20))) ; default health value

;; define a component specifying that an entity is able to be controlled by the player.
(defcomponent player-controlled
  ((value t)))

;;; define some systems
;;; important: the order in which systems are defined, is the order in which they will be
;;; iterated through during the game loop.

;; define a system that handles user input events -- requires the health and player-controlled
;; components.
(defsys handle-input (health player-controlled))

;; define a system that handles damage done to entities -- requires the health component.
(defsys damage (health))

;; create an entity and add the health component to it.
;; in this case, the entity's health will use the default value of 20 in the definition.
(let ((e (make-entity)))
 (add-component e 'health))

;; create an entity and add the health and player-controlled components.
;; in this case, we override the default value for the health component.
(let ((e (make-entity)))
 (add-component e 'health :value 100)
 (add-component e 'player-controlled))

;; define a silly method for the damage system, that simply decrements all entities health by 1
;; each iteration.
;; note: this will be called for all entities that possess the health component as in the defsys
;; definition.
(defmethod do-system ((system damage) entity)
 (decf (value (component entity 'health))))

;; define a method for the handle-input system.
;; note: this will be called for all entities that possess the player-controlled component as in
;; the defsys definition.
(defmethod do-system ((system handle-input) entity)
  "Your player input event code goes here")

;; step through all systems one time.
;; note: you would normally call this in your game loop to be called many times per second.
(tick)
```
