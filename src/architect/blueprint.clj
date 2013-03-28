(ns ^{:doc "Define blueprints for validating maps, sequences, sets or any
            arbitrary data structures or values."}
  architect.blueprint
  (:require [architect.internal.utils :as u]
            [clojure.set :as set]))


;; Questions asked of Blueprints

(defn blueprint?
  "Returns whether x is a blueprint"
  [x]
  (cond (var? x) (contains? (meta x) ::blueprint)
        (map? x) (contains? x :blueprint-spec)
        :else false))

(defn strict-blueprint?
  "Only makes sense to call on a map-blueprint.
   Returns whether a blueprint is strict.  A strict blueprint necessitates that
the map under-validation has all the paths listed in the blueprint and no extra paths"
  [x]
  (cond (var? x) (and (blueprint? x) (::strict (meta x)))
        (map? x) (and (blueprint? x) (:strict x))
        :else false))

(defn loose-blueprint?
  "Only makes sense to call on a map-blueprint.
   Returns whether a blueprint is loose.  A loose blueprint allows the
map under-validation to have more keys than are specified in the blueprint."
  [x]
  (cond (var? x) (and (blueprint? x) (not (::strict (meta x))))
        (map? x) (and (blueprint? x) (not (:strict x)))
        :else false))

(defn blueprint-rows
  "Only makes sense to call on a map-blueprint.
   Returns a sequence of pairs, where the first element is the
   path and the second element is the blueprint that applies to that path."
  [blueprint]
  (partition 2 (:blueprint-spec blueprint)))

(defn blueprint-path-set
  "Only makes sense to call on a map-blueprint.
   Returns the set of all paths in the blueprint."
  [blueprint]
  (set (take-nth 2 (:blueprint-spec blueprint))))

(defn- constraint-bundle? [x]
  (and (map? x)
       (contains? x ::constraint-bundle)))


;;; Blueprints For Arbitrary Data Structures or Values: Simple Blueprints

(defn class-blueprint
  "Creates a blueprint that states the item should be
   an instance of the supplied Class.
   Can be used for any arbitrary data structure or value."
  [clazz]
  {:type :class
   :blueprint-spec clazz
   :constraints []})

(defn or-statement-blueprint
  "Creates a blueprint that states the item should match
   at least one of the supplied blueprints.
   Can be used for any arbitrary data structure or value."
  [blueprints]
  {:type :or-statement
   :blueprint-spec blueprints
   :constraints []})

(defn and-statement-blueprint
  "Creates a blueprint that states the item should match
   ALL of the supplied blueprints.
   Can be used for any arbitrary data structure or value."
  [and-statement]
  {:type :and-statement
   :blueprint-spec and-statement
   :constraints []})

(defn predicate-blueprint
  "Creates a blueprint that states the item should match
   the supplied predicate.
   Can be used for any arbitrary data structure or value."
  ([pred]
     (predicate-blueprint pred nil))
  ([pred source]
     {:type :predicate
      :blueprint-spec pred
      :constraints []
      :source source}))

(defn regex-blueprint
  "Creates a blueprint that states the item should match the supplied
   regex, using `re-find` semantics."
  [regex]
  {:type :predicate
   :blueprint-spec #(re-find regex %)
   :source  (list 'fn '[s] (list 're-find (symbol (str "#\"" regex "\"")) 's))
   :constraints []})

(defn simple-blueprint
  "Makes a simple blueprint from x.
   If x is a Class, makes a class-blueprint.
   If x looks like [:or x y], makes an or-statement-blueprint.
   If x looks like [x y], makes an and-statement-blueprint.
   Otherwise, makes a predicate blueprint.

   Can be used for any arbitrary data structure or value."
  [x]
  {:pre [(not (blueprint? x))]}
  (cond (instance? java.util.regex.Pattern x) (regex-blueprint x)
        (class? x) (class-blueprint x)
        (and (vector? x) (= :or (first x))) (or-statement-blueprint (rest x))
        (vector? x) (and-statement-blueprint x)
        (or (fn? x)
            (map? x)
            (set? x)) (predicate-blueprint x)
            :else (predicate-blueprint (partial = x)
                                       (list 'fn '[x] (list '= x 'x)))))

(defn ensure-blueprint
  "Takes any x.  If it is already a blueprint, returns it, otherwise attempts to
   create a simple blueprint from it."
  [x]
  (if (blueprint? x) x (simple-blueprint x)))

(defmacro def-simple-blueprint
  "Creates a named var for a simple-blueprint.  See `simple-blueprint` for more details."
  [name x]
  `(def ~(vary-meta name assoc ::blueprint true)
     (simple-blueprint ~x)))

;;;; Blueprints for Specific Data Structures

(defn- constraint-form [sub-blueprint-sexp]
  `(let [sss# ~sub-blueprint-sexp]
     (assoc (if (blueprint? sss#) sss# (simple-blueprint sss#))
       :source '~sub-blueprint-sexp)))

(defmacro ^{:private true} constraint [sub-blueprint-sexp]
  (constraint-form sub-blueprint-sexp))

(defmacro constraints
  "Wrap a group of sub-blueprints, so that they can be tested against
   the entire data structure in a surrounding blueprint."
  [& sub-blueprint-sexps]
  {::constraint-bundle (vec (map constraint-form sub-blueprint-sexps))})

(def ^{:private true} map-constraints [(constraint [:or nil? map?])])
(def ^{:private true} seq-constraints [(constraint [:or nil? sequential?])])
(def ^{:private true} set-constraints [(constraint [:or nil? set?])])

(defn map-blueprint
  "Creates a blueprint for a map. looseness is either :loose or :strict. If :strict
   then, any keys not mentioned in the blueprint-spec will be errors.
   Can be supplied other blueprints which it will addd behavior to..
   Accepts constraints that are applied to the whole map."
  [looseness & constraints-and-blueprint-vectors]
  (let [user-specified-constraints (mapcat ::constraint-bundle
                                           (filter constraint-bundle? constraints-and-blueprint-vectors))
        blueprints (filter blueprint? constraints-and-blueprint-vectors)
        inherited-blueprint-specs (mapcat :blueprint-spec blueprints)
        inherited-constraints (mapcat :constraints blueprints)
        blueprint-spec? (fn [x] (and (vector? x) (not (constraint-bundle? x))))
        blueprint-specs (apply concat (filter blueprint-spec? constraints-and-blueprint-vectors))
        flattened-blueprint-specs (vec (concat inherited-blueprint-specs blueprint-specs))
        compiled-blueprint-specs (u/map-nth 2 ensure-blueprint flattened-blueprint-specs)]
    (assert (even? (count blueprint-specs)))
    (assert (every? sequential? (blueprint-path-set {:blueprint-spec blueprint-specs})))
    {:type :map
     :blueprint-spec compiled-blueprint-specs
     :constraints (distinct (concat map-constraints inherited-constraints user-specified-constraints))
     :strict (= :strict looseness)}))

(defmacro def-map-blueprint
  "Creates a named var for a map-blueprint, defaults to being strict.  Can also be
   made loose by passing in :loose as the first parameter.  See `map-blueprint` for more details."
  {:arglists '([name & constraints-and-blueprint-vectors]
                 [looseness name & constraints-and-blueprint-vectors])}
  [& args]
  (let [[looseness name & constraints-and-blueprint-vectors] (if (keyword? (first args))
                                                               args
                                                               (cons :strict args))]
    (assert (contains? #{:strict :loose} looseness))
    `(def ~(vary-meta name assoc ::blueprint true ::strict (= :strict looseness))
       (map-blueprint ~looseness ~@constraints-and-blueprint-vectors))))

(defn seq-blueprint
  "Creates a blueprint for a sequence. Every element of the sequence should match
   the given blueprint.
   First argument is either :all or :layout, and indicates whether to apply the
   given blueprint to all members of the sequence, or to treat the supplied vector as
   a layout to check the sequence against.
   Accepts constraints that are applied to the whole sequence."
  [all-or-layout & constraints-and-blueprint-specs]
  (let [user-specified-constraints (mapcat ::constraint-bundle
                                           (filter constraint-bundle? constraints-and-blueprint-specs))
        blueprint (first (remove constraint-bundle? constraints-and-blueprint-specs))
        seq-layout blueprint]
    (assert (contains? #{:all :layout} all-or-layout))
    (if (= :layout all-or-layout)
      {:type :seq-layout
       :blueprint-spec (vec (map ensure-blueprint seq-layout))
       :constraints (distinct (concat seq-constraints
                                      user-specified-constraints
                                      [(constraint (fn [xs] (= (count seq-layout) (count xs))))]))}
      {:type :seq
       :blueprint-spec (ensure-blueprint blueprint)
       :constraints (distinct (concat seq-constraints user-specified-constraints))})))

(defmacro def-seq-blueprint
  "Creates a named var for a seq-blueprint. See `seq-blueprint` for more details."
  {:arglists '([name & constraints-and-blueprint-specs]
                 [all-or-layout name & constraints-and-blueprint-specs])}
  [& args]
  (let [[all-or-layout name & constraints-and-blueprint-specs] (if (keyword? (first args))
                                                                 args
                                                                 (cons :all args))]
    (assert (contains? #{:all :layout} all-or-layout))
    `(def ~(vary-meta name assoc ::blueprint true)
       (seq-blueprint ~all-or-layout ~@constraints-and-blueprint-specs))))

(defn set-blueprint
  "Creates a blueprint for a set. Every element of the set should match
   the given blueprint.
   Accepts constraints that are applied to the whole sequence."
  [& constraints-and-blueprint-specs]
  (let [user-specified-constraints (mapcat ::constraint-bundle
                                           (filter constraint-bundle? constraints-and-blueprint-specs))
        blueprint (first (remove constraint-bundle? constraints-and-blueprint-specs))]
    {:type :set
     :blueprint-spec (ensure-blueprint blueprint)
     :constraints (concat set-constraints user-specified-constraints)}))

(defmacro def-set-blueprint
  "Creates a named var for a set-blueprint. See `set-blueprint` for more details."
  [name & constraints-and-blueprint-specs]
  `(def ~(vary-meta name assoc ::blueprint true)
     (set-blueprint ~@constraints-and-blueprint-specs)))

(defn sequence-of
  "Wraps a blueprint to make it a blueprint that apply to every element of a sequential"
  [blueprint]
  (seq-blueprint :all blueprint))

(def ^{:doc "Wraps a blueprint to make it a blueprint that apply to every element of a set"}
  set-of set-blueprint)

(defn ->string-blueprint
  "Wraps a blueprint. Returns a new blueprint that is a String that when read matches
   the wrapped blueprint.

   Ex. (validation-errors (->string-blueprint (sequence-of Long)) \"[55, -33]\")
       ;; => #{}"
  [blueprint]
  (assoc (ensure-blueprint blueprint) :pre-validation-transform #'read-string))


;; Wildcard Paths

(defn wildcard?
  "Returns whether x is a wilcard"
  [x]
  (= true (::wildcard x)))

(defn wild
  "Wraps a blueprint or simple-blueprint to be used within a map-blueprint path as a wildcard.
   Ex. [:a (wild Integer) (wild String)], matches all paths like [:a 1 \"product-1\"] or [:a 42 \"product-2\"]"
  [blueprint]
  {::wildcard true
   :blueprint blueprint})

(defn wildcard-path?
  "Returns whether or not a path is a wildcard-path"
  [blueprint-path]
  (some wildcard? blueprint-path))

(defn wildcard-path-set
  "Return the set of all wildcard paths in the blueprint"
  [blueprint]
  (set/select wildcard-path? (blueprint-path-set blueprint)))


;;;; Schema Path Modifiers

(defn optional-path
  "Takes a blueprint path and morphs it into a path that is optional.
   Optional paths may or may not be present on the validated map, but
   if they are present they must be valid against the given blueprint."
  [blueprint-path]
  (vary-meta blueprint-path assoc ::optional-path true))

(defn optional-path?
  "Returns whether or not a path is an optional-path"
  [blueprint-path]
  (boolean (::optional-path (meta blueprint-path))))

(defn optional-path-set
  "Set of all optional-paths in a blueprint"
  [blueprint]
  (set/select optional-path? (blueprint-path-set blueprint)))


;; Filtering Map-Blueprints

(defn filter-blueprint
  "Only makes sense to call on a map-blueprint.
   Takes a pred like (fn [[path blueprint]] ...) and selects all blueprint rows that match."
  [pred blueprint]
  (assoc blueprint :blueprint-spec (->> (blueprint-rows blueprint)
                                        (filter pred)
                                        (apply concat)
                                        vec)))

(defn subtract-paths
  "Only makes sense to call on a map-blueprint.
   Returns a new blueprint minus some paths."
  [blueprint & paths]
  (filter-blueprint (fn [[path _]] (not (contains? (set paths) path)))
                    blueprint))

(defn select-blueprint-keys
  "Only makes sense to call on a map-blueprint.
   Returns a new blueprint with only the paths starting with the specified keys."
  [blueprint & ks]
  (filter-blueprint (fn [[path _]] (contains? (set ks) (first path)))
                    blueprint))

(defn subtract-wildcard-paths
  "Only makes sense to call on a map-blueprint..
   Returns a blueprint that is the same in all respects, except it has none of the wildcard paths."
  [blueprint]
  (filter-blueprint (fn [[path _]] (not (wildcard-path? path)))
                    blueprint))


;;;; Namespace Info

(defn ns->blueprints
  "All blueprints in a namespace"
  [the-ns]
  (filter blueprint? (vals (ns-interns the-ns))))


;;;; Scaffolding

(defn scaffold-blueprint
  "Makes a simple scaffolding blueprint from a given map, sequence or set."
  [blueprint-name x]
  (cond (map? x)
        (list 'def-map-blueprint (symbol blueprint-name)
              (vec (interleave (sort (u/paths x))
                               (repeat 'Anything))))

        (set? x)
        (list 'def-set-blueprint (symbol blueprint-name)
              'Anything)

        (sequential? x)
        (list 'def-seq-blueprint (symbol blueprint-name)
              'Anything)))

