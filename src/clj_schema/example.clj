(ns ^{:doc "Ways to create example test data that must matches a given schema"}
  clj-schema.example
  (:require [clj-schema.internal.utils :as u]
            [clj-schema.validation :as v]
            [clojure.pprint :as pprint]))


(defn example
  "Takes a map and a schema, if it fails validation it throws an
exception, else returns the map untouched"
  [schema example-map]
  (v/validate-and-handle example-map
                         schema
                         identity
                         (fn [fx-map error-msgs]
                           (pprint/pprint fx-map)
                           (throw (AssertionError. (str "Schema validation failed: " error-msgs))))))

(defmacro def-example
  "Defines a var whose value is the result of calling `example` on the schema and map"
  [name schema example-map]
  `(def ~name (example ~schema ~example-map)))

(defmacro def-example-factory
  "Like defn, except the result of evaluating the function is checked
   for validity aginst the supplied schema. Supports multi-arity definitions."
  [name schema & lists-of-args+bodies]
  (let [multi-arity? (not (vector? (first lists-of-args+bodies)))
        kw-args? (and (not multi-arity?)
                      (map? (last (first lists-of-args+bodies))) ;; last arg is a map
                      (= '& (last (butlast (first lists-of-args+bodies)))))] ;; second to last is '&
    (cond multi-arity?
          `(defn ~name
             ~@(for [[args & body-that-creates-example] lists-of-args+bodies]
                 `(~args
                   (example ~schema (do ~@body-that-creates-example)))))

           kw-args?
           (let [[args & body-that-creates-example] lists-of-args+bodies]
             `(u/defn-kw ~name ~args
                (example ~schema (do ~@body-that-creates-example))))

          :else
          `(def-example-factory ~name ~schema ~lists-of-args+bodies)))) ;; turns single-arity into multi-arity