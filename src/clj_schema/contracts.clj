(ns clj-schema.contracts
  "Unobtrusively apply contracts to functions vars"
  (:use [clj-schema.schema :refer [def-map-schema optional-path schema? sequence-of]]
        [clj-schema.simple-schemas :refer [Anything]]
        [clj-schema.validation :refer [validation-errors]])
  (:require [robert.hooke :as hooke]))


(def-map-schema ^:private contract-schema :loose
  [[:var] var?
   (optional-path [:sampling-rate]) [:or nil fn? [number? #(>= % 0) #(<= % 100)]]
   (optional-path [:input-schema]) Anything
   (optional-path [:input-schema-on-failure]) [:or nil fn?]
   (optional-path [:output-schema]) Anything
   (optional-path [:output-schema-on-failure]) [:or nil fn?]])

(defn- check? [sampling-rate args]
  (cond (not sampling-rate) true
        (fn? sampling-rate) (> (apply sampling-rate args) (rand 100))
        :else               (> sampling-rate (rand 100))))

(defn- schema-checker-fn [{:keys [var
                                  sampling-rate
                                  input-schema
                                  input-schema-on-failure
                                  input-schema-on-success
                                  output-schema
                                  output-schema-on-failure
                                  output-schema-on-success]}]
  (fn [f & args]
    (let [check? (check? sampling-rate args)]
      (when check?
        (let [errors (and input-schema (validation-errors input-schema args))]
          (if (seq errors)
            (if input-schema-on-failure
              (input-schema-on-failure var (vec args) errors)
              (throw (Exception. (str "Errors found in inputs, " (vec args) ", to " var ": " errors))))
            (when input-schema-on-success
              (input-schema-on-success var (vec args))))))

      (let [result (apply f args)]
        (when check?
          (let [errors (and output-schema (validation-errors output-schema result))]
            (if (seq errors)
              (if output-schema-on-failure
                (output-schema-on-failure var result errors)
                (throw (Exception. (str "Errors found in outputs, " result ", from " var ": " errors))))
              (when output-schema-on-success
                (output-schema-on-success var result)))))
        result))))

(defn add-contracts!
  "Wrap vars specified in contract maps such that they check
   inputs and outputs against supplied schemas.

   Example fully-decked-out contract:

   {:var #'f
    :sampling-rate 50 ;; 0-100 (percent)
    ;; or ... :sampling-rate (fn [a b c] (sampling-rate a b c))
    ;;    ...  can take a fn here that gets the args sent to the fn (#'f)
    :input-schema (schema/sequence-of [:or String clojure.lang.Keyword])
    :input-schema-on-failure (fn [f input errors]
                               (log/error [f input errors]))
    :input-schema-on-success (fn [f input]
                               (log/info [f input]))
    :output-schema String
    :output-schema-on-failure (fn [f result errors]
                                (log/error [f result errors]))
    :output-schema-on-success (fn [f result]
                                (log/info [f result]))}"
  [contracts]
  (when-let [errors (seq (validation-errors (sequence-of contract-schema) contracts))]
    (throw (Exception. (str "contracts were not valid: " contracts errors))))
  (doseq [c contracts]
    (hooke/add-hook (:var c) ::contract (schema-checker-fn c))))

(defn remove-contracts!
  "Removes all contracts that were added by calling clj-schema.contracts/add-contracts!"
  [contracts]
  (when-let [errors (seq (validation-errors (sequence-of contract-schema) contracts))]
    (throw (Exception. (str "contracts were not valid: " contracts errors))))
  (doseq [c contracts]
    (hooke/remove-hook (:var c) ::contract)))
