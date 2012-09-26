(ns ^{:doc "Attempt to force the types of a given map into the
            Classes specified in a schema"}
  clj-schema.coerce
  (:require [clj-schema.schema :as sch])
  (:import clojure.lang.Keyword))



;; TODO - Alex - use util libraries instead?
(defn- as-str [x]
  (if (instance? clojure.lang.Named x)
    (name x)
    (str x)))

(defn- contains-path? [m path]
  (and (not (empty? path))
       (not= ::not-found (get-in m path ::not-found))))

(defn- update-in-if-present [m path f]
  (if (contains-path? m path)
    (update-in m path f)
    m))




(declare coerce-map)

(defmulti coerce-value
  (fn [validator _x_]
    (cond (sch/schema? validator)      :schema
          (sequential? validator)      :and-or-or-statement
          (sch/sequence-of? validator) :sequence
          :else                        validator)))

(defmethod coerce-value :schema [schema x]
  (coerce-map schema x))

(defmethod coerce-value :sequence [seq-validator xs]
  (let [validator (:single-item-validator seq-validator)]
    (map #(coerce-value validator %) xs)))

(defmethod coerce-value :and-or-or-statement [statement x]
  (let [schema (first (filter sch/schema? statement))
        clazz  (first (filter class? (flatten statement)))]
    (if schema
      (coerce-value schema x)
      (coerce-value clazz x))))

(defmethod coerce-value String [_to-class_ x]
  (when x
    (as-str x)))

(defmethod coerce-value Keyword [_to-class_ x]
  (cond (nil? x)     nil
        (keyword? x) x
        :else        (keyword (str x))))

(defmethod coerce-value :default [_ x]
  x)

(defn coerce-map [schema m]
  (when-not (empty? (sch/wildcard-path-set schema))
    (throw (IllegalArgumentException. (str "Does not support type coercion for schemas with wildcards.\n" schema))))
  (reduce (fn [coerced-m [path validator]]
            (update-in-if-present coerced-m path (partial coerce-value validator)))
          m
          (sch/schema-rows schema)))