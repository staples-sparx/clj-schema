(ns ^{:doc "Ways to create fixture test data that must match a given schema"}
  clj-schema.fixtures
  (:require [clj-schema.schema :as schema]
            [clojure.pprint :as pprint]))

(defn- assert-no-validation-errors [fixture-map error-msgs]
  (when-not (empty? error-msgs)
    (pprint/pprint fixture-map)
    (throw (AssertionError. (str "Schema validation failed: " error-msgs)))))

(defn fixture [schema fixture-map]
  (assert-no-validation-errors fixture-map (schema/validation-errors schema fixture-map))
  fixture-map)

(defmacro def-fixture [name schema fixture-map]
  `(def ~name (fixture ~schema ~fixture-map)))

(defmacro def-fixture-factory
  "like defn, except the result of evaluating the function is checked
   for validity aginst the supplied schema. Supports multi-arity definitions."
  [name schema & lists-of-args+bodies]
  (if (vector? (first lists-of-args+bodies))
    ;; single arity def -- get the fn for the macro so I can call it again to make the
    ;;                     single-arity effectively a call to the multi-arity version
    (#'def-fixture-factory nil nil name schema lists-of-args+bodies)

    ;; multi-arity defn
    `(defn ~name
       ~@(for [[args & body-that-creates-fixture] lists-of-args+bodies]
           `(~args
              (fixture ~schema (do ~@body-that-creates-fixture)))))))