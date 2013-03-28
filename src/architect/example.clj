(ns ^{:doc "Ways to create example test data that must match a given blueprint"}
  architect.example
  (:require [architect.internal.utils :as u]
            [architect.validation :as v]
            [clojure.pprint :as pprint]))


(defn example
  "Takes a map and a blueprint, if it fails validation it throws an
exception, else returns the map untouched"
  [blueprint example-map]
  (v/validate-and-handle example-map
                         blueprint
                         identity
                         (fn [fx-map error-msgs]
                           (pprint/pprint fx-map)
                           (throw (AssertionError. (str "Schema validation failed: " error-msgs))))))

(defmacro def-example
  "Defines a var whose value is the result of calling `example` on the blueprint and map"
  [name blueprint example-map]
  `(def ~name (example ~blueprint ~example-map)))

(defmacro def-example-factory
  "Like defn, except the result of evaluating the function is checked
   for validity aginst the supplied blueprint. Supports multi-arity definitions."
  [name blueprint & lists-of-args+bodies]
  (let [multi-arity? (not (vector? (first lists-of-args+bodies)))
        kw-args? (and (not multi-arity?)
                      (map? (last (first lists-of-args+bodies))) ;; last arg is a map
                      (= '& (last (butlast (first lists-of-args+bodies)))))] ;; second to last is '&
    (cond multi-arity?
          `(defn ~name
             ~@(for [[args & body-that-creates-example] lists-of-args+bodies]
                 `(~args
                   (example ~blueprint (do ~@body-that-creates-example)))))

          kw-args?
          (let [[args & body-that-creates-example] lists-of-args+bodies]
            `(u/defn-kw ~name ~args
               (example ~blueprint (do ~@body-that-creates-example))))

          :else
          `(def-example-factory ~name ~blueprint ~lists-of-args+bodies)))) ;; turns single-arity into multi-arity