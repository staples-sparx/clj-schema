(ns ^{:doc "Ways to create fixture test data that must match a given schema"}
  clj-schema.fixtures
  (:require [clj-schema.schema :as schema]))

(defn- assert-no-validation-errors [fixture-map error-msgs]
  (when-not (empty? error-msgs)
    (clojure.pprint/pprint fixture-map)
    (throw (AssertionError. (str "Schema validation failed: " error-msgs)))))

(defn fixture [schema fixture-map]
  (assert-no-validation-errors fixture-map (schema/validation-errors schema fixture-map))
  fixture-map)

(defmacro def-fixture [name schema fixture-map]
  `(def ~name (fixture ~schema ~fixture-map)))

(defmacro def-fixture-factory [name schema args & body-that-creates-fixture]
  `(defn ~name ~args
     (let [fixture-map# (do ~@body-that-creates-fixture)]
       (fixture ~schema fixture-map#))))