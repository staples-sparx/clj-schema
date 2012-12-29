(ns ^{:doc
      "Define validation schemas for validating maps.

      Schemas are any number of paths through a nested map, paired with a validator.

      There are 5 types of validators:
      * any predicate function
      * any Class object, i.e. String, clojure.lang.Keyword, java.util.Date, etc
      * any clj-schema schema
      * [validator1 validator2] to indicate both validator1 AND validator2
      * [:or validator1 validator2] to indicate both validator1 OR validator2

      Any validator may be wrapped in sequence-of to indicate the value should be
      sequential, or wrapped in set-of to indicate the value is a set. By default,
      schemas assume the value at a path to be a singular value.

      A path may be marked as an `optional-path`.  This means that is doesn't
      have to be present, but if it is, it must match the given validator.

      Wildcard paths are paths where one or more peices are defined as anything
      matching a given validator.

       Example Schema:

      (def-map-schema bar-schema
       [[:a :b :c] pred
       [:x :y :z] [pred2 pred3 z-schema] ;; implicit 'and' - all three must pass
       [:p :q :r] [:or nil? r-schema]    ;; an 'or' statement - need just one to pass
       (optional-path [:z]) (sequence-of string?)
       [:a b :d] (loose-valdiation-schema [[:cat :name] String ;; can use Java Class objects directly
                                           [:cat :colors] (set-of String)])
      ... ]

     Example schema w/ wildcard paths:

      (def-map-schema foo-schema
        [[:a (wild String) (wild Number)] String])

      => matches maps such as:
         {:a {\"car\" {1.21 \"jigawatts\"}}}
         {:a {\"banana\" {2 \"green\"

      `def-map-schema` creates a strict schema, which expects only the paths it
      describes to be present on the given map.

      `def-loose-schema` creates a loose schema, which expects its paths to
      be present but does not complain about extra paths."}
  clj-schema.schema
  (:require [clj-schema.internal.utils :as u]
            [clojure.set :as set]))


;; Questions asked of Schemas

(defn schema?
  "Returns whether x is a schema"
  [x]
  (cond (var? x) (contains? (meta x) ::schema)
        (map? x) (contains? x :schema-spec)
        :else false))

(defn strict-schema?
  "Returns whether a schema is strict.  A strict schema necessitates that
the map under-validation has all the paths listed in the schema and no extra paths" 
  [x]
  (cond (var? x) (and (schema? x) (::strict (meta x)))
        (map? x) (and (schema? x) (:strict x))
        :else false))

(defn loose-schema?
  "Returns whether a schema is loose.  A loose schema allows the
map under-validation to have more keys than are specified in the schema."
  [x]
  (cond (var? x) (and (schema? x) (not (::strict (meta x))))
        (map? x) (and (schema? x) (not (:strict x)))
        :else false))

(defn schema-rows
  "Returns a sequence of pairs, where the first element is the
path and the second element is the validator"
  [schema]
  (partition 2 (:schema-spec schema)))

(defn schema-path-set
  "Returns the set of all paths in the schema."
  [schema]
  (set (take-nth 2 (:schema-spec schema))))

(defn constraint?
  "Returns whether x is a constraint."
  [x]
  (and (map? x)
       (= #{:predicate :source} (set (keys x)))))

(defn constraints?
  "Returns whether x is a seq of only constraints."
  [x]
  (and (sequential? x)
       (every? constraint? x)))


;;;; Schema Creation

(defn as-loose-schema
  "Removes :strict-schema true k/v pair from the given schema,
   making it validate loosely"
  [schema]
  (assoc schema :strict false))

(defn as-strict-schema
  "Adds :strict-schema true k/v pair to the given schema,
   making it validate strictly"
  [schema]
  (assoc schema :strict true))

(defmacro constraints
  "Wrap a group of predicates, so that they can be tested against
   the entire map."
  [& pred-sexps]
  (vec (for [ps pred-sexps]
         `{:predicate ~ps
           :source '~ps})))

(def ^{:doc "Constraints common to all map schemas"}
  map-constraints (constraints (fn [m] (or (nil? m) (map? m)))))

(def ^{:doc "Constraints common to all seq schemas"}
  seq-constraints (constraints (fn [m] (or (nil? m) (sequential? m)))))

(def ^{:doc "Constraints common to all set schemas"}
  set-constraints (constraints (fn [m] (or (nil? m) (set? m)))))

(defn map-schema
  "TODO"
  [looseness & constraints-and-schema-vectors]
  (let [user-specified-constraints (apply concat (filter constraints? constraints-and-schema-vectors))
        flattened-schemas (mapcat :schema-spec (filter schema? constraints-and-schema-vectors))
        schema-spec? (fn [x] (and (vector? x) (not (constraints? x))))
        schema-specs (apply concat (filter schema-spec? constraints-and-schema-vectors))
        flattened-schema-specs (vec (concat flattened-schemas schema-specs))]
    (assert (even? (count schema-specs)))
    (assert (every? sequential? (schema-path-set {:schema-spec schema-specs})))
    {:type :map
     :schema-spec flattened-schema-specs
     :constraints (concat map-constraints user-specified-constraints)
     :strict (= :strict looseness)}))

(defmacro def-map-schema
  "Creates a named var for a strict schema TODO"
  [& args]
  {:arglists '([name & constraints-and-schema-vectors]
               [looseness name & constraints-and-schema-vectors])}
  (let [[looseness name & constraints-and-schema-vectors] (if (keyword? (first args))
                                                            args
                                                            (cons :strict args))
        _ (assert (contains? #{:strict :loose} looseness))]
    `(-> (def ~name (map-schema ~looseness ~@constraints-and-schema-vectors))
         (alter-meta! assoc ::schema true ::strict ~(= :strict looseness)))))

(defn seq-schema
  "TODO"
  [& constraints-and-schema-specs]
  (let [user-specified-constraints (apply concat (filter constraints? constraints-and-schema-specs))
        validator (first (remove constraints? constraints-and-schema-specs))]
    ;; TODO: unit test in some validations of these args
    {:type :seq
     :schema-spec validator
     :constraints (concat seq-constraints user-specified-constraints)}))

(defmacro def-seq-schema
  "TODO"
  [name & constraints-and-schema-specs]
  `(def ~name (seq-schema ~@constraints-and-schema-specs)))

(defn set-schema
  "TODO"
  [& constraints-and-schema-specs]
  (let [user-specified-constraints (apply concat (filter constraints? constraints-and-schema-specs))
        validator (first (remove constraints? constraints-and-schema-specs))]
    ;; TODO: unit test in some validations of these args
    {:type :set
     :schema-spec validator
     :constraints (concat set-constraints user-specified-constraints)}))

(defmacro def-set-schema
  "TODO"
  [name & constraints-and-schema-specs]
  `(def ~name (set-schema ~@constraints-and-schema-specs)))


;; Validator Modifiers

(def ^{:doc "Wraps a validator to make it a validator that apply to every element of a sequential"}
  sequence-of seq-schema)

(def ^{:doc "Wraps a validator to make it a validator that apply to every element of a set"}
  set-of set-schema)

(defrecord WildcardValidator [validator])

(defn wildcard-validator?
  "Returns whether a validator is a wilcard"
  [validator]
  (= WildcardValidator (class validator)))

(defn wild
  "Wraps a validator to be used within a path as a wildcard.
   Ex. [:a (wild Integer) (wild String)], matches all paths like [:a 1 \"product-1\"] or [:a 42 \"product-2\"]"
  [validator]
  (WildcardValidator. validator))

(defn wildcard-path?
  "Returns whether or not a path is a wildcard-path"
  [schema-path]
  (some wildcard-validator? schema-path))

(defn wildcard-path-set
  "Return the set of all wildcard paths in the schema"
  [schema]
  (set/select wildcard-path? (schema-path-set schema)))


;;;; Schema Path Modifiers

(defn optional-path
  "Takes a schema path and morphs it into a path that is optional.
   Optional paths may or may not be present on the validated map, but
   if they are present they must be valid against the given validator."
  [schema-path]
  (vary-meta schema-path assoc ::optional-path true))

(defn optional-path?
  "Returns whether or not a path is an optional-path"
  [schema-path]
  (boolean (::optional-path (meta schema-path))))

(defn optional-path-set
  "Set of all optional-paths in a schema"
  [schema]
  (set/select optional-path? (schema-path-set schema)))


;; Filtering Schemas

(defn filter-schema
  "Takes a pred like (fn [[path validator]] ...) and selects all schema rows that match."
  [pred schema]
  (assoc schema :schema-spec (->> (schema-rows schema)
                                  (filter pred)
                                  (apply concat)
                                  vec)))

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

(defn subtract-wildcard-paths
  "Returns a schema that is the same in all respects, except it has none of the wildcard paths."
  [schema]
  (filter-schema (fn [[path validator]] (not (wildcard-path? path)))
                      schema))


;;;; Namespace Info

(defn ns->schemas
  "All schemas in a namespace"
  [the-ns]
  (filter schema? (vals (ns-interns the-ns))))


;;;; Scaffolding

(defn scaffold-schema
  "Makes a simple scaffolding schema from a given map m.
   Each path has a validator of Anything."
  [schema-name m]
  (list 'def-map-schema (symbol schema-name)
        (vec (interleave (sort (u/paths m))
                         (repeat 'Anything)))))

