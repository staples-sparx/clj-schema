(ns ^{:doc "Ways to create fixture test data that must match a given schema"}
  clj-schema.fixtures
  (:require [clj-schema.validation :as v]
            [clojure.pprint :as pprint]))


(defn fixture [schema fixture-map]
  (v/validate-and-handle fixture-map
                         schema
                         identity
                         (fn [fx-map error-msgs]
                           (pprint/pprint fx-map)
                           (throw (AssertionError. (str "Schema validation failed: " error-msgs))))))

(defmacro def-fixture [name schema fixture-map]
  `(def ~name (fixture ~schema ~fixture-map)))

(defmacro def-fixture-factory
  "like defn, except the result of evaluating the function is checked
   for validity aginst the supplied schema. Supports multi-arity definitions."
  [name schema & lists-of-args+bodies]
  (let [multi-arity? (not (vector? (first lists-of-args+bodies)))]
    (if multi-arity?
      `(defn ~name
         ~@(for [[args & body-that-creates-fixture] lists-of-args+bodies]
             `(~args
               (fixture ~schema (do ~@body-that-creates-fixture)))))
      `(def-fixture-factory ~name ~schema ~lists-of-args+bodies)))) ;; turns single-arity into multi-arity