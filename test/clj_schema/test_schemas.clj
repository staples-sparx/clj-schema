(ns clj-schema.test-schemas
  (:use [clj-schema.schema]))


(defschema name-schema   [[:name :first] String])
(defschema height-schema [[:height] Number])
(defschema count-schema [[:count] Number])
(defschema product-schema [[:quantity] Number
                           [:price]    Number])
(def-loose-schema loose-height-schema [[:height] Number])
(defschema person-schema
  name-schema
  height-schema)

(def-loose-schema loose-person-schema
  [[:name :first] String
   [:height] Number])

(defschema family-schema
  [[:mom] person-schema
   [:dad] person-schema])

(defschema mom-strict-dad-loose-family-schema
  [[:mom] person-schema
   [:dad] loose-person-schema])

(def-loose-schema schema-with-constraints
  (constraints (comp even? count distinct vals)
               (fn [m] (even? (count (keys m)))))
  [[:a] String
   [:b] Number])