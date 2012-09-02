(ns ^{:doc
      "Define validation schemas for validating maps.

      Schemas are any number of paths through a nested map, paired with a validator.

      Validators are either a single predicate or schema, or a seq of predicates
      or schems (or a mix of predicates or schemas).

      A path may also be marked as an `optional-path`. By default a value at a given
      path is assumed to be a single value; to mark it sequential, wrap the validator
      in the 'sequence-of' function. NOTE: 'nil' is a passing value for a 'sequence-of' validator.

      Example Schema:

      [[:a :b :c] pred
       [:x :y :z] [pred2 pred3 z-schema] ;; implicit 'and' - all three must pass
       [:p :q :r] [:or nil? r-schema]    ;; an 'or' statement - need just one to pass
       (optional-path [:z]) (sequence-of string?)
       [:a b :d] (loose-valdiation-schema [[:cat :name] String ;; can use Java Class objects directly
                                           [:cat :color] String])
      ... ]

      `def-validation-schema` creates a strict schema, which expects only the paths it
      describes to be present on the given map.

      `def-loose-validation-schema` creates a loose schema, which expects its paths to
      be present but does not complain about extra paths."}
  clj-schema.schema
  (:use clj-schema.utils)
  (:require [clojure.set :as set]))


;;;; Validation Schema Creation

(defn- schema-path-set [schema]
  (set (take-nth 2 schema)))

(defn loose-validation-schema
  "From a seq of vectors, creates a schema that can be used within other schemas.
   Checks for the presence of all paths; other paths may also exist."
  [& vs]
  {:pre [(even? (count (apply concat vs)))
         (every? vector? (schema-path-set (apply concat vs)))]}
  (let [schema-vector (vec (apply concat vs))]
    (vary-meta schema-vector assoc :validation/schema true)))

(defn strict-validation-schema
  "From a seq of vectors, creates a schema that can be used within other schemas.
   Any paths found in addition to the ones specified are considered a violation."
  [& vs]
  {:pre [(even? (count (apply concat vs)))
         (every? vector? (schema-path-set (apply concat vs)))]}
  (let [schema-vector (vec (apply concat vs))]
    (vary-meta schema-vector merge {:validation/schema true
                                    :validation/strict-schema true})))

(defmacro def-loose-validation-schema
  "Creates a named var for a loose schema that can be used within other schemas."
  [name & schema-vectors]
  `(-> (def ~name (loose-validation-schema ~@schema-vectors))
     (alter-meta! assoc :validation/schema true)))

(defmacro def-validation-schema
  "Creates a named var for a strict schema that can be used within other schemas."
  [name & schema-vectors]
  `(-> (def ~name (strict-validation-schema ~@schema-vectors))
     (alter-meta! merge {:validation/schema true
                         :validation/strict-schema true})))


;; Questions asked of Schemas

(defn schema? [x]
  (boolean (:validation/schema (meta x))))

(defn strict-schema? [x]
  (boolean (and (schema? x)
             (:validation/strict-schema (meta x)))))

(defn loose-schema? [x]
  (boolean (and (schema? x)
             (not (:validation/strict-schema (meta x))))))

(defn schema-rows [schema]
  (partition 2 schema))

(defn num-schema-paths [schema]
  (count (schema-rows schema)))

;; Filtering Schemas

(defn filter-schema
  "Takes a pred like (fn [[path validator]] ...) and selects all schema rows that match."
  [pred schema]
  (let [new-schema (->> (schema-rows schema)
    (filter pred)
    (apply concat)
    vec)]
    (with-meta new-schema (meta schema))))

(defn subtract-paths
  "Returns a new schema minus some paths."
  [schema & paths]
  (filter-schema (fn [[path validator]] (not (contains? (set paths) path)))
    schema))

(defn select-schema-keys
  "Returns a new schema with only the paths starting with the specified keys."
  [schema & ks]
  (filter-schema (fn [[path validator]] (contains? (set ks) (first path)))
    schema))

(declare wildcard-path?)
(defn subtract-wildcard-paths
  "Returns a schema that is the same in all respects, except it has none of the wildcard paths."
  [schema]
  (filter-schema (fn [[path validator]] (not (wildcard-path? path)))
    schema))

;; Validator Modifiers

;; I can't use metadata to tag things as being validators that are meant to be run against
;; a sequential value, because Java Class objects cannot have metadata applied to them

; sequence-of
(defrecord SequenceOfItemsValidator [single-item-validator])

(defn sequence-of? [validator]
  (= SequenceOfItemsValidator (class validator)))

(defn sequence-of [single-item-validator]
  (SequenceOfItemsValidator. single-item-validator))

; wild
(defrecord WildcardValidator [validator])

(defn wildcard-validator? [validator]
  (= WildcardValidator (class validator)))

(defn wild
  "Upgrades a validator to be used within a path as a wildcard.
   Ex. [:a (wild Integer) (wild String)], matches all paths like [:a 1 \"product-1\"] or [:a 42 \"product-2\"]"
  [validator]
  (WildcardValidator. validator))

;;;; Schema Path Modifiers

(defn optional-path
  "Takes a schema path and morphs it into a path that is optional.
   Optional paths may or may not be present on the validated map, but
   if they are present they must be valid against the given validator."
  [schema-path]
  (vary-meta schema-path assoc :validation/optional-path true))

(defn optional-path? [schema-path]
  (boolean (:validation/optional-path (meta schema-path))))

(defn wildcard-path? [schema-path]
  (some wildcard-validator? schema-path))


;;;; Determining whether a map has any validation errors -- main entry point is 'validation-errors' so start there

(declare validation-errors)

;; Types of errors - these abstract away the errors themselves, so
;;                   we could make these errors be maps instead straightforwardly
(defn- non-map-error [parent-path m]
  (format "At path %s, expected a map, got %s instead." parent-path (pr-str m)))

(defn- extraneous-path-error [xtra-path]
  (format "Path %s was not specified in the schema." xtra-path))

(defn- missing-path-error [missing-path]
  (format "Map did not contain expected path %s." missing-path))

(defn- sequential-val-error [values-at-path full-path]
  (format "Map value %s, at path %s, was a single value but was tagged with 'sequence-of'."
    (pr-str values-at-path) full-path))

(defn- single-val-error [value full-path]
  (format "Map value %s, at path %s, was sequential but not tagged with 'sequence-of'."
    (pr-str value) full-path))

(defn- predicate-fail-error [val-at-path full-path pred]
  (format "Map value %s, at path %s, did not match predicate '%s'."
    (pr-str val-at-path) full-path (verbose-fn-name pred)))

(defn- instance-of-fail-error [val-at-path full-path expected-class]
  (format "Map value %s at path %s expected class %s, but was %s"
    (pr-str val-at-path) full-path (pr-str expected-class) (pr-str (class val-at-path))))
;;

(defn- validator-type [validator]
  (cond (sequence-of? validator) :sequence
    (schema? validator) :schema
    (class? validator) :class
    (and (sequential? validator) (= :or (first validator))) :or-statement
    (sequential? validator) :and-statement
    :else :predicate))

(defmulti errors-for-path-content #(validator-type %3))

(defmethod errors-for-path-content :schema [full-path val-at-path schema]
  (if (sequential? val-at-path)
    [(single-val-error val-at-path full-path)]
    (validation-errors full-path schema val-at-path)))

(defmethod errors-for-path-content :class [full-path val-at-path expected-class]
  (cond (sequential? val-at-path)
    [(single-val-error val-at-path full-path)]

    (not (instance? expected-class val-at-path))
    [(instance-of-fail-error val-at-path full-path expected-class)]

    :else
    []))

(defmethod errors-for-path-content :predicate [full-path val-at-path pred]
  (cond (sequential? val-at-path)
    [(single-val-error val-at-path full-path)]

    (not ((fn->fn-thats-false-if-throws pred) val-at-path))  ;; keeps us safe from ClassCastExceptions, etc
    [(predicate-fail-error val-at-path full-path pred)]

    :else
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

(defmethod errors-for-path-content :sequence [full-path values-at-path validator]
  (if (or (nil? values-at-path) (sequential? values-at-path))
    (mapcat #(errors-for-path-content full-path % (:single-item-validator validator)) values-at-path)
    [(sequential-val-error values-at-path full-path)]))

(defn- matches-validator? [validator x]
  (empty? (errors-for-path-content [] x validator)))

;; TODO - ALEX July 30, move to some utils ns
(defn- safe-keys [x]
  (when (map? x)
    (keys x)))

(defn wildcard-path->concrete-paths [m [path-first & path-rest :as the-wildcard-path]]
  (if (empty? the-wildcard-path)
    [[]]
    (let [keys-that-match-validator (if (wildcard-validator? path-first)
      (filter #(matches-validator? (:validator path-first) %) (safe-keys m))
      [path-first])]
      (for [k-that-matches-validator keys-that-match-validator
            one-of-the-concrete-path-ends (wildcard-path->concrete-paths (get m k-that-matches-validator) path-rest)]
        (vec (cons k-that-matches-validator one-of-the-concrete-path-ends))))))

(defn- errors-for-concrete-path [m parent-path schema-path validator]
  (let [val-at-path (get-in m schema-path ::not-found)
        contains-path? (not= ::not-found val-at-path)
        full-path (into parent-path schema-path)]
    (cond
      (and (not contains-path?) (optional-path? schema-path))
      []

      (not contains-path?)
      [(missing-path-error full-path)]

      :else
      (errors-for-path-content full-path val-at-path validator))))

(defn- errors-for-possibly-wildcard-path [m parent-path schema-path validator]
  (if (wildcard-path? schema-path)
    (let [concrete-paths (wildcard-path->concrete-paths
      m
      schema-path)
          concrete-paths (if (optional-path? schema-path) (map optional-path concrete-paths) concrete-paths)]
      (mapcat #(errors-for-concrete-path m parent-path % validator) concrete-paths))
    (errors-for-concrete-path m parent-path schema-path validator)))


(defn- path-content-errors [parent-path schema m]
  (->> (schema-rows schema)
    (mapcat (fn [[schema-path validator]]
              (errors-for-possibly-wildcard-path m parent-path schema-path validator)))
    set))

(defn- shorten-to-schema-path-set
  "Since the result of calling 'paths' will be paths that go deeper into
   the map than our schema may specify, we truncate the result of calling
   'paths' to the longest version of them that is included in the schema's path set."
  [all-paths schema-path-set]
  (set (for [path all-paths]
         (if-let [validated-subpath (last (filter schema-path-set (subpaths path)))]
           validated-subpath
           path))))

(defn remove-subpaths [paths]
  (let [all-subpaths (distinct (mapcat subpaths paths))
        any-of-all-subpaths-is-super-path? (fn [p]
      (some #(and (subpath? p %) (not= p %))
        all-subpaths))]
    (remove any-of-all-subpaths-is-super-path? paths)))

(defn- extraneous-paths [schema m]
  (let [schema-paths (set (remove-subpaths (schema-path-set schema)))
        shortened (shorten-to-schema-path-set (paths m) schema-paths)]
    (set/difference shortened schema-paths)))

(defn covered-by-wildcard-path? [[path-first & path-rest :as path-to-check] [wildcard-first & wildcard-rest :as wildcard-path]]
  (if-not (= (count path-to-check) (count wildcard-path)) ;; optimization
    false
    (cond
      (empty? path-to-check)
      true

      (wildcard-validator? wildcard-first)
      (if (matches-validator? (:validator wildcard-first) path-first)
        (covered-by-wildcard-path? path-rest wildcard-rest)
        false)

      :else
      (if (= wildcard-first path-first)
        (covered-by-wildcard-path? path-rest wildcard-rest)
        false))))

(defn matches-any-wildcard-path? [all-wild-card-paths path]
  (some (partial covered-by-wildcard-path? path) all-wild-card-paths))

(defn- wildcard-paths [schema]
  (filter wildcard-path? (schema-path-set schema)))

(defn- extraneous-paths-errors [parent-path all-wildcard-paths schema m]
  (if (loose-schema? schema)
      #{}
    (set (for [xtra-path (extraneous-paths schema m)
               :when (not-any? (partial matches-any-wildcard-path? all-wildcard-paths)
        (subpaths xtra-path))]
           (extraneous-path-error (into parent-path xtra-path))))))

;; TODO: ALEX - September 1, 2012 -- extract third namespace clj-schema.validation for all validating code
(defn validation-errors
  "Returns a set of all the validation errors found when comparing a given
   map m, against the supplied schema.

   A validator is either a schema, predicate, Class or vector of them.
   See this ns's :doc meta for more details."
  ([schema m]
    (validation-errors [] schema m))
  ([parent-path schema m]
    (if-not (or (nil? m) (map? m))
        #{(non-map-error parent-path m)}
      (set/union (path-content-errors parent-path schema m)

        ;; check for wildcard extra paths specially above
        (extraneous-paths-errors parent-path  (wildcard-paths schema) (subtract-wildcard-paths schema) m)))))

(defn valid? [schema m]
  (empty? (validation-errors schema m)))


;;;; Namespace Info

(defn ns->schemas
  "All schemas in a namespace"
  [the-ns]
  (filter schema? (vals (ns-interns the-ns))))

