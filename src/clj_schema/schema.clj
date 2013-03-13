(ns ^{:doc "Define schemas for validating maps, sequences, sets or any
            arbitrary data structures or values."}
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
  "Only makes sense to call on a map-schema.
   Returns whether a schema is strict.  A strict schema necessitates that
the map under-validation has all the paths listed in the schema and no extra paths" 
  [x]
  (cond (var? x) (and (schema? x) (::strict (meta x)))
        (map? x) (and (schema? x) (:strict x))
        :else false))

(defn loose-schema?
  "Only makes sense to call on a map-schema.
   Returns whether a schema is loose.  A loose schema allows the
map under-validation to have more keys than are specified in the schema."
  [x]
  (cond (var? x) (and (schema? x) (not (::strict (meta x))))
        (map? x) (and (schema? x) (not (:strict x)))
        :else false))

(defn schema-rows
  "Only makes sense to call on a map-schema.
   Returns a sequence of pairs, where the first element is the
   path and the second element is the schema that applies to that path."
  [schema]
  (partition 2 (:schema-spec schema)))

(defn schema-path-set
  "Only makes sense to call on a map-schema.
   Returns the set of all paths in the schema."
  [schema]
  (set (take-nth 2 (:schema-spec schema))))

(defn- constraint-bundle? [x]
  (and (map? x)
       (contains? x ::constraint-bundle)))


;;; Schemas For Arbitrary Data Structures or Values: Simple Schemas

(defn class-schema
  "Creates a schema that states the item should be
   an instance of the supplied Class.
   Can be used for any arbitrary data structure or value."
  [clazz]
  {:type :class
   :schema-spec clazz
   :constraints []})

(defn or-statement-schema
  "Creates a schema that states the item should match
   at least one of the supplied schemas.
   Can be used for any arbitrary data structure or value."
  [schemas]
  {:type :or-statement
   :schema-spec schemas
   :constraints []})

(defn and-statement-schema
  "Creates a schema that states the item should match
   ALL of the supplied schemas.
   Can be used for any arbitrary data structure or value."
  [and-statement]
  {:type :and-statement
   :schema-spec and-statement
   :constraints []})

(defn predicate-schema
  "Creates a schema that states the item should match
   the supplied predicate.
   Can be used for any arbitrary data structure or value."
  [pred]
  {:type :predicate
   :schema-spec pred
   :constraints []})

(defn regex-schema
  "Creates a schema that states the item should match the supplied
   regex, using `re-find` semantics."
  [regex]
  {:type :predicate
   :schema-spec #(re-find regex %)
   :source  (list 'fn '[s] (list 're-find (symbol (str "#'\"" regex "\"")) 's))
   :constraints []})

(defn simple-schema
  "Makes a simple schema from x.
   If x is a Class, makes a class-schema.
   If x looks like [:or x y], makes an or-statement-schema.
   If x looks like [x y], makes an and-statement-schema.
   Otherwise, makes a predicate schema.

   Can be used for any arbitrary data structure or value."
  [x]
  {:pre [(not (schema? x))]}
  (cond (instance? java.util.regex.Pattern x) (regex-schema x)
        (class? x) (class-schema x)
        (and (vector? x) (= :or (first x))) (or-statement-schema (rest x))
        (vector? x) (and-statement-schema x)
        :else (predicate-schema x)))

(defn- ensure-schema [x]
  (if (schema? x) x (simple-schema x)))

(defmacro def-simple-schema
  "Creates a named var for a simple-schema.  See `simple-schema` for more details."
  [name x]
  `(-> (def ~name (simple-schema ~x))
       (alter-meta! assoc ::schema true)))


;;;; Schemas for Specific Data Structures

(defn- constraint-form [sub-schema-sexp]
  `(let [sss# ~sub-schema-sexp]
     (assoc (if (schema? sss#) sss# (simple-schema sss#))
       :source '~sub-schema-sexp)))

(defmacro ^{:private true} constraint [sub-schema-sexp]
  (constraint-form sub-schema-sexp))

(defmacro constraints
  "Wrap a group of sub-schemas, so that they can be tested against
   the entire data structure in a surrounding schema."
  [& sub-schema-sexps]
  {::constraint-bundle (vec (map constraint-form sub-schema-sexps))})

(def ^{:private true} map-constraints [(constraint [:or nil? map?])])
(def ^{:private true} seq-constraints [(constraint [:or nil? sequential?])])
(def ^{:private true} set-constraints [(constraint [:or nil? set?])])

(defn map-schema
  "Creates a schema for a map. looseness is either :loose or :strict. If :strict
   then, any keys not mentioned in the schema-spec will be errors.
   Can be supplied other schemas which it will addd behavior to..
   Accepts constraints that are applied to the whole map."
  [looseness & constraints-and-schema-vectors]
  (let [user-specified-constraints (mapcat ::constraint-bundle
                                           (filter constraint-bundle? constraints-and-schema-vectors))
        schemas (filter schema? constraints-and-schema-vectors)
        inherited-schema-specs (mapcat :schema-spec schemas)
        inherited-constraints (mapcat :constraints schemas)
        schema-spec? (fn [x] (and (vector? x) (not (constraint-bundle? x))))
        schema-specs (apply concat (filter schema-spec? constraints-and-schema-vectors))
        flattened-schema-specs (vec (concat inherited-schema-specs schema-specs))
        compiled-schema-specs (u/map-nth 2 ensure-schema flattened-schema-specs)]
    (assert (even? (count schema-specs)))
    (assert (every? sequential? (schema-path-set {:schema-spec schema-specs})))
    {:type :map
     :schema-spec compiled-schema-specs
     :constraints (distinct (concat map-constraints inherited-constraints user-specified-constraints))
     :strict (= :strict looseness)}))

(defmacro def-map-schema
  "Creates a named var for a map-schema, defaults to being strict.  Can also be
   made loose by passing in :loose as the first parameter.  See `map-schema` for more details."
  {:arglists '([name & constraints-and-schema-vectors]
               [looseness name & constraints-and-schema-vectors])}
  [& args]
  (let [[looseness name & constraints-and-schema-vectors] (if (keyword? (first args))
                                                            args
                                                            (cons :strict args))]
    (assert (contains? #{:strict :loose} looseness))
    `(-> (def ~name (map-schema ~looseness ~@constraints-and-schema-vectors))
         (alter-meta! assoc ::schema true ::strict ~(= :strict looseness)))))

(defn seq-schema
  "Creates a schema for a sequence. Every element of the sequence should match
   the given schema.
   First argument is either :all or :layout, and indicates whether to apply the
   given schema to all members of the sequence, or to treat the supplied vector as
   a layout to check the sequence against.
   Accepts constraints that are applied to the whole sequence."
  [all-or-layout & constraints-and-schema-specs]
  (let [user-specified-constraints (mapcat ::constraint-bundle
                                           (filter constraint-bundle? constraints-and-schema-specs))
        schema (first (remove constraint-bundle? constraints-and-schema-specs))
        seq-layout schema]
    (assert (contains? #{:all :layout} all-or-layout))
    (if (= :layout all-or-layout)
      {:type :seq-layout
       :schema-spec (vec (map ensure-schema seq-layout))
       :constraints (distinct (concat seq-constraints
                                      user-specified-constraints
                                      [(constraint (fn [xs] (= (count seq-layout) (count xs))))]))}
      {:type :seq
       :schema-spec (ensure-schema schema)
       :constraints (distinct (concat seq-constraints user-specified-constraints))})))

(defmacro def-seq-schema
  "Creates a named var for a seq-schema. See `seq-schema` for more details."
  {:arglists '([name & constraints-and-schema-specs]
               [all-or-layout name & constraints-and-schema-specs])}
  [& args]
  (let [[all-or-layout name & constraints-and-schema-specs] (if (keyword? (first args))
                                                            args
                                                            (cons :all args))]
    (assert (contains? #{:all :layout} all-or-layout))
    `(-> (def ~name (seq-schema ~all-or-layout ~@constraints-and-schema-specs))
         (alter-meta! assoc ::schema true))))

(defn set-schema
  "Creates a schema for a set. Every element of the set should match
   the given schema.
   Accepts constraints that are applied to the whole sequence."
  [& constraints-and-schema-specs]
  (let [user-specified-constraints (mapcat ::constraint-bundle
                                           (filter constraint-bundle? constraints-and-schema-specs))
        schema (first (remove constraint-bundle? constraints-and-schema-specs))]
    {:type :set
     :schema-spec (ensure-schema schema)
     :constraints (concat set-constraints user-specified-constraints)}))

(defmacro def-set-schema
  "Creates a named var for a set-schema. See `set-schema` for more details."
  [name & constraints-and-schema-specs]
  `(-> (def ~name (set-schema ~@constraints-and-schema-specs))
       (alter-meta! assoc ::schema true)))

(defn sequence-of
  "Wraps a schema to make it a schema that apply to every element of a sequential"
  [schema]
  (seq-schema :all schema))

(def ^{:doc "Wraps a schema to make it a schema that apply to every element of a set"}
  set-of set-schema)


;; Wildcard Paths

(defn wildcard?
  "Returns whether x is a wilcard"
  [x]
  (= true (::wildcard x)))

(defn wild
  "Wraps a schema or simple-schema to be used within a map-schema path as a wildcard.
   Ex. [:a (wild Integer) (wild String)], matches all paths like [:a 1 \"product-1\"] or [:a 42 \"product-2\"]"
  [schema]
  {::wildcard true
   :schema schema})

(defn wildcard-path?
  "Returns whether or not a path is a wildcard-path"
  [schema-path]
  (some wildcard? schema-path))

(defn wildcard-path-set
  "Return the set of all wildcard paths in the schema"
  [schema]
  (set/select wildcard-path? (schema-path-set schema)))


;;;; Schema Path Modifiers

(defn optional-path
  "Takes a schema path and morphs it into a path that is optional.
   Optional paths may or may not be present on the validated map, but
   if they are present they must be valid against the given schema."
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


;; Filtering Map-Schemas

(defn filter-schema
  "Only makes sense to call on a map-schema.
   Takes a pred like (fn [[path schema]] ...) and selects all schema rows that match."
  [pred schema]
  (assoc schema :schema-spec (->> (schema-rows schema)
                                  (filter pred)
                                  (apply concat)
                                  vec)))

(defn subtract-paths
  "Only makes sense to call on a map-schema.
   Returns a new schema minus some paths."
  [schema & paths]
  (filter-schema (fn [[path _]] (not (contains? (set paths) path)))
                     schema))

(defn select-schema-keys
  "Only makes sense to call on a map-schema.
   Returns a new schema with only the paths starting with the specified keys."
  [schema & ks]
  (filter-schema (fn [[path _]] (contains? (set ks) (first path)))
                      schema))

(defn subtract-wildcard-paths
  "Only makes sense to call on a map-schema..
   Returns a schema that is the same in all respects, except it has none of the wildcard paths."
  [schema]
  (filter-schema (fn [[path _]] (not (wildcard-path? path)))
                      schema))


;;;; Namespace Info

(defn ns->schemas
  "All schemas in a namespace"
  [the-ns]
  (filter schema? (vals (ns-interns the-ns))))


;;;; Scaffolding

(defn scaffold-schema
  "Makes a simple scaffolding schema from a given map, sequence or set."
  [schema-name x]
  (cond (map? x)
        (list 'def-map-schema (symbol schema-name)
              (vec (interleave (sort (u/paths x))
                               (repeat 'Anything))))

        (set? x)
        (list 'def-set-schema (symbol schema-name)
              'Anything)

        (sequential? x)
        (list 'def-seq-schema (symbol schema-name)
              'Anything)))

