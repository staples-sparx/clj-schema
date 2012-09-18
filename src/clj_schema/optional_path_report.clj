(ns clj-schema.optional-path-report
  (:require [clj-schema.schema :as sch]))

(defn optional-path-report [schema m]
  {:all-optional-paths (sch/optional-path-set schema)
   :present #{}
   :absent #{}})