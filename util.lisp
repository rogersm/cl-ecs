(in-package :cl-ecs)

(defun all (list1 list2)
  "Check if all items in LIST1 are in LIST2."
  (and (intersection list1 list2)
       (not (set-difference list1 list2))))

(defun any (list1 list2)
  "Check if any items in LIST1 are in LIST2."
  (when (intersection list1 list2) t))
