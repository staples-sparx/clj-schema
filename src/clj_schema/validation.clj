(ns clj-schema.validation
  (:require [clojure.set :as set]
            [clj-schema.schema :as s]
            [clj-schema.internal.utils :as u]))


(defprotocol ErrorReporter
  "Factory functions to generate each type of validation error.

   First arg 'state' is a map with 4 keys:
     :map-under-validation - the map being validated
     :schema - the schema being used to validate
     :parent-path - the path if any to the current map, from a map that contained this one
     :full-path - the full path in a nested map structure
     :all-wildcard-paths - any path that includes a wildcard
     :schema-without-wildcard-paths - a version of the schema with wildcard paths removed"
  (constraint-error [this state constraint] "Caused by a constraint predicate failing against the entire map")
  (extraneous-path-error [this state xtra-path] "Caused by finding a path that doesn't exist in the schema.  This only applies to schemas that are not loose")
  (missing-path-error [this state missing-path] "Caused by not finding a path mentioned in the schema")
  (predicate-fail-error [this state val-at-path pred] "Caused by a predicate validator returning false or nil")
  (instance-of-fail-error [this state val-at-path expected-class] "Caused by the value not being the expected Class, and not being a subtype of the expected Class"))

(deftype StringErrorReporter []
  ErrorReporter
  (constraint-error [_ {:keys [parent-path map-under-validation]} constraint]
    (format "At path %s, constraint failed. Expected '(%s %s)' to be true, but was false."
            parent-path (:source constraint) (pr-str map-under-validation)))
  
  (extraneous-path-error [_ _ xtra-path]
    (format "Path %s was not specified in the schema." xtra-path))
  
  (missing-path-error [_ _ missing-path]
    (format "Map did not contain expected path %s." missing-path))
  
  (predicate-fail-error [_ {:keys [full-path]} val-at-path pred]
    (format "Value %s, at path %s, did not match predicate '%s'."
            (pr-str val-at-path) full-path (u/pretty-fn-str pred)))
  
  (instance-of-fail-error [_ {:keys [full-path]} val-at-path expected-class]
    (format "Value %s at path %s expected class %s, but was %s"
            (pr-str val-at-path) full-path (pr-str expected-class) (pr-str (class val-at-path)))))

;; used to hold state of one `validation-errors` calculation
(def ^{:private true :dynamic true} *error-reporter* nil)
(def ^{:private true :dynamic true} *map-under-validation* nil)
(def ^{:private true :dynamic true} *schema* nil)
(def ^{:private true :dynamic true} *parent-path* nil)
(def ^{:private true :dynamic true} *all-wildcard-paths* nil)
(def ^{:private true :dynamic true} *schema-without-wildcard-paths* nil)

(defn- state-map-for-reporter [full-path]
  {:map-under-validation *map-under-validation*
   :schema *schema*
   :parent-path *parent-path*
   :full-path full-path
   :all-wildcard-paths *all-wildcard-paths*
   :schema-without-wildcard-paths *schema-without-wildcard-paths*})

(defn- validator-type [validator]
  (cond (s/schema? validator) :schema
        (class? validator) :class
        (and (sequential? validator) (= :or (first validator))) :or-statement
        (sequential? validator) :and-statement
        :else :predicate))

(declare validation-errors)

(defmulti ^{:private true} errors-for-path-content #(validator-type %3))

(defmethod errors-for-path-content :schema [full-path val-at-path schema]
  (validation-errors *error-reporter* full-path schema val-at-path))

(defmethod errors-for-path-content :class [full-path val-at-path expected-class]
  (if-not (instance? expected-class val-at-path)
    [(instance-of-fail-error *error-reporter* (state-map-for-reporter full-path) val-at-path expected-class)]
    []))

(defmethod errors-for-path-content :predicate [full-path val-at-path pred]
  (if-not ((u/fn->fn-thats-false-if-throws pred) val-at-path)  ;; keeps us safe from ClassCastExceptions, etc
    [(predicate-fail-error *error-reporter* (state-map-for-reporter full-path) val-at-path pred)]
    []))

(defmethod errors-for-path-content :and-statement [full-path val-at-path validators]
  (let [error-msgs (mapcat (partial errors-for-path-content full-path val-at-path) validators)]
    (if-not (zero? (count error-msgs))
      error-msgs
      [])))

(defmethod errors-for-path-content :or-statement [full-path val-at-path [_:or_ & validators]]
  (let [error-msg-batches (map (partial errors-for-path-content full-path val-at-path) validators)
        error-msgs        (apply concat error-msg-batches)]
    (if-not (< (count (remove empty? error-msg-batches))
               (count validators))
      error-msgs
      [])))

(defn- matches-validator? [validator x]
  (empty? (errors-for-path-content [] x validator)))

(defn- safe-keys [x]
  (when (map? x)
    (keys x)))

(defn- wildcard-path->concrete-paths [m [path-first & path-rest :as the-wildcard-path]]
  (if (empty? the-wildcard-path)
    [[]]
    (let [keys-that-match-validator (if (s/wildcard-validator? path-first)
                                      (filter #(matches-validator? (:validator path-first) %) (safe-keys m))
                                      [path-first])]
      (for [k-that-matches-validator keys-that-match-validator
            one-of-the-concrete-path-ends (wildcard-path->concrete-paths (get m k-that-matches-validator) path-rest)]
        (vec (cons k-that-matches-validator one-of-the-concrete-path-ends))))))

(defn- errors-for-concrete-path [schema-path validator]
  (let [val-at-path (get-in *map-under-validation* schema-path ::not-found)
        contains-path? (not= ::not-found val-at-path)
        full-path (into *parent-path* schema-path)]
    (cond (and (not contains-path?) (s/optional-path? schema-path))
          []
          
          (not contains-path?)
          [(missing-path-error *error-reporter* (state-map-for-reporter full-path) full-path)]
          
          :else
          (errors-for-path-content full-path val-at-path validator))))

(defn- errors-for-possibly-wildcard-path [schema-path validator]
  (if (s/wildcard-path? schema-path)
    (let [concrete-paths (wildcard-path->concrete-paths *map-under-validation* schema-path)
          ;; TODO ALex - Sep 15, 2012 - this is here because metadata lost - add abstraction to keep metadata for schemas across a translation
          concrete-paths (if (s/optional-path? schema-path) (map s/optional-path concrete-paths) concrete-paths)]
      (mapcat #(errors-for-concrete-path % validator) concrete-paths))
    (errors-for-concrete-path schema-path validator)))


(defn- path-content-errors []
  (->> (s/schema-rows *schema*)
       (mapcat (fn [[schema-path validator]]
                 (errors-for-possibly-wildcard-path schema-path validator)))
       set))

(defn- shorten-to-schema-path-set
  "Since the result of calling 'paths' will be paths that go deeper into
   the map than our schema may specify, we truncate the result of calling
   'paths' to the longest version of them that is included in the schema's path set."
  [all-paths schema-path-set]
  (set (for [path all-paths]
         (if-let [validated-subpath (last (filter schema-path-set (u/subpaths path)))]
           validated-subpath
           path))))

(defn- remove-subpaths [paths]
  (let [all-subpaths (distinct (mapcat u/subpaths paths))
        any-of-all-subpaths-is-super-path? (fn [p]
                                             (some #(and (u/subpath? p %) (not= p %))
                                                   all-subpaths))]
    (remove any-of-all-subpaths-is-super-path? paths)))

(defn- extraneous-paths []
  (let [schema-paths (set (remove-subpaths (s/schema-path-set *schema-without-wildcard-paths*)))
        shortened (shorten-to-schema-path-set (u/paths *map-under-validation*) schema-paths)]
    (set/difference shortened schema-paths)))

(defn- covered-by-wildcard-path? [[path-first & path-rest :as path-to-check] [wildcard-first & wildcard-rest :as wildcard-path]]
  (let [path-to-check-count (count path-to-check)]
    (cond (not= path-to-check-count (count wildcard-path))
          false

          (zero? path-to-check-count)
          true

          (s/wildcard-validator? wildcard-first)
          (if (matches-validator? (:validator wildcard-first) path-first)
            (covered-by-wildcard-path? path-rest wildcard-rest)
            false)

          :else
          (covered-by-wildcard-path? path-rest wildcard-rest))))

(defn- matches-any-wildcard-path? [path]
  (some (partial covered-by-wildcard-path? path) *all-wildcard-paths*))

(defn- extraneous-paths-errors []
  (if (s/loose-schema? *schema-without-wildcard-paths*)
    #{}
    (set (for [xtra-path (extraneous-paths)
               :when (not-any? matches-any-wildcard-path? (u/subpaths xtra-path))]
           (extraneous-path-error *error-reporter*
                                  (state-map-for-reporter *parent-path*)
                                  (into *parent-path* xtra-path))))))

(defn- constraint-errors []
  (set (for [c (:constraints *schema*)
             :when (not ((:predicate c) *map-under-validation*))]
         (constraint-error *error-reporter* (state-map-for-reporter []) c))))


(defn- map-validation-errors [parent-path schema m]
  (binding [*all-wildcard-paths* (s/wildcard-path-set schema)
            *schema-without-wildcard-paths* (s/subtract-wildcard-paths schema)]
    (set/union (path-content-errors)
               (extraneous-paths-errors))))

(defn- seq-validation-errors [parent-path schema xs]
  (let [validator (:schema-spec schema)
        map-fn (fn [idx x]
                 (errors-for-path-content parent-path x validator))]
    (->> (map-indexed map-fn xs)
         (apply concat)
         set)))

(defn- set-validation-errors [parent-path schema xs]
  (seq-validation-errors parent-path schema xs))

(defn validation-errors
  "Returns a set of all the validation errors found when comparing a given
   map m, against the supplied schema.

   A validator is either a schema, predicate, Class or vector of them.
   See this ns's :doc meta for more details."
  ([schema m]
     (validation-errors (StringErrorReporter.) [] schema m))
  ([error-reporter schema m]
     (validation-errors error-reporter [] schema m))
  ([error-reporter parent-path schema m]
     (binding [*error-reporter* error-reporter
               *map-under-validation* m
               *schema* schema
               *parent-path* parent-path]
       (if-let [c-errors (seq (constraint-errors))]
         (set c-errors)
         (case (:type schema)
           :map (map-validation-errors parent-path schema m)
           :seq (seq-validation-errors parent-path schema m)
           :set (set-validation-errors parent-path schema m))))))

(defn valid?
  "Returns true if calling `validation-errors` would return no errors"
  ([schema m]
    (valid? (StringErrorReporter.) schema m))
  ([error-reporter schema m]
    (empty? (validation-errors error-reporter schema m))))

(defn validate-and-handle
  "Validates map m vs a schema.
   If it passes, then calls success-handler-fn passing m to it.
   If it fails, then calls error-handler-fn passing m and any validation errors to it."
  ([error-reporter m schema success-handler-fn error-handler-fn]
    (if-let [errors (seq (validation-errors error-reporter schema m))]
      (error-handler-fn m errors)
      (success-handler-fn m)))
  ([m schema success-handler-fn error-handler-fn]
    (validate-and-handle (StringErrorReporter.) m schema success-handler-fn error-handler-fn)))