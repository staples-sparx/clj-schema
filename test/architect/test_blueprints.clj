(ns architect.test-blueprints
  (:use [architect.blueprint]
        [architect.simple-blueprints]))


(def-map-blueprint name-blueprint   [[:name :first] String])
(def-map-blueprint height-blueprint [[:height] Number])
(def-map-blueprint count-blueprint [[:count] Number])
(def-map-blueprint product-blueprint [[:quantity] Number
                                      [:price]    Number])
(def-map-blueprint :loose loose-height-blueprint
  [[:height] Number])
(def-map-blueprint person-blueprint
  name-blueprint
  height-blueprint)

(def-map-blueprint :loose loose-person-blueprint
  [[:name :first] String
   [:height] Number])

(def-map-blueprint family-blueprint
  [[:mom] person-blueprint
   [:dad] person-blueprint])

(def-map-blueprint mom-strict-dad-loose-family-blueprint
  [[:mom] person-blueprint
   [:dad] loose-person-blueprint])

(def-map-blueprint :loose blueprint-with-constraints
  (constraints (comp even? count distinct vals)
               (fn [m] (even? (count (keys m)))))
  [[:a] String
   [:b] Number])

(def-seq-blueprint my-seq-blueprint
  String)

(def-set-blueprint my-set-blueprint
  Number)

(def-simple-blueprint my-simple-blueprint
  String)

;;; Checkerboard

(def black-square (OneOf 0))
(def white-square (OneOf 1))

(def-seq-blueprint :layout white-row
  [white-square black-square white-square black-square white-square black-square white-square black-square])

(def-seq-blueprint :layout black-row
  [black-square white-square black-square white-square black-square white-square black-square white-square])

(def-seq-blueprint :layout checkers-board-blueprint
  [white-row black-row white-row black-row white-row black-row white-row black-row])


;;; 

(def-map-blueprint :loose non-empty-map
  (constraints (simple-blueprint map?)
               (complement empty?)))

(def-map-blueprint unsorted-non-empty-map
  non-empty-map
  [[:a] (OneOf 1)]
  (constraints (fn [m] (not (sorted? m)))))

(def-seq-blueprint :all red-list
  (constraints (fn [xs] (even? (count xs))))
  (OneOf :red)
  (constraints list?))

(def-set-blueprint red-set
  (constraints (fn [xs] (even? (count xs))))
  (OneOf :red :RED :Red)
  (constraints sorted?))