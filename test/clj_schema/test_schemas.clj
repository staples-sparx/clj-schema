(ns clj-schema.test-schemas
  (:use [clj-schema.schema]))


(def-map-schema name-schema   [[:name :first] String])
(def-map-schema height-schema [[:height] Number])
(def-map-schema count-schema [[:count] Number])
(def-map-schema product-schema [[:quantity] Number
                           [:price]    Number])
(def-map-schema :loose loose-height-schema
  [[:height] Number])
(def-map-schema person-schema
  name-schema
  height-schema)

(def-map-schema :loose loose-person-schema
  [[:name :first] String
   [:height] Number])

(def-map-schema family-schema
  [[:mom] person-schema
   [:dad] person-schema])

(def-map-schema mom-strict-dad-loose-family-schema
  [[:mom] person-schema
   [:dad] loose-person-schema])

(def-map-schema :loose schema-with-constraints
  (constraints (comp even? count distinct vals)
               (fn [m] (even? (count (keys m)))))
  [[:a] String
   [:b] Number])

(def-seq-schema my-seq-schema
  String)

(def-set-schema my-set-schema
  Number)

(def-simple-schema my-simple-schema
  String)

