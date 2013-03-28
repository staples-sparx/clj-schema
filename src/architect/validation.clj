(ns architect.validation
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [architect.blueprint :as s]
            [architect.internal.utils :as u]))


(defprotocol ErrorReporter
  "Factory functions to generate each type of validation error.

   First arg 'state' is a map with 4 keys:
     :data-under-validation - the data being validated
     :blueprint - the blueprint being used to validate
     :parent-path - the path if any to the current map, from a map that contained this one
     :full-path - the full path in a nested map structure
     :all-wildcard-paths - any path that includes a wildcard
     :blueprint-without-wildcard-paths - a version of the blueprint with wildcard paths removed"
  (constraint-error [this state constraint]
    "Caused by a constraint predicate failing against the entire data structure or value")
  (extraneous-path-error [this state xtra-path]
    "Caused by finding a path that doesn't exist in the blueprint.  This only applies to blueprints that are not loose")
  (missing-path-error [this state missing-path]
    "Caused by not finding a path mentioned in the blueprint")
  (predicate-fail-error [this state val-at-path pred]
    "Caused by a predicate blueprint returning false or nil")
  (instance-of-fail-error [this state val-at-path expected-class]
    "Caused by the value not being the expected Class, and not being a subtype of the expected Class")
  (pre-validation-transform-error [this state val-at-path pre-validation-transform-fn]
    "Caused by the value not being able to be transformed into a validatable form"))

(defn- intelli-print [blueprint f]
  (or (:source blueprint)
      (u/pretty-fn-str f)))

(defn- prefixed [pre-validation-transform-fn original s]
  (if pre-validation-transform-fn
    (str "After applying :pre-validation-transform of " pre-validation-transform-fn " to original of " (pr-str original) ", " (apply str (str/lower-case (first s)) (rest s)))
    s))

(deftype StringErrorReporter []
  ErrorReporter
  (constraint-error [_ {:keys [parent-path pre-validation-transform]} constraint]
    (if (empty? parent-path)
      (format "Constraint failed: '%s'" (:source constraint))
      (format "At parent path %s, constraint failed: '%s'"
              parent-path (:source constraint))))

  (extraneous-path-error [_ {:keys [pre-validation-transform original]} xtra-path]
    (prefixed pre-validation-transform original
              (format "Path %s was not specified in the blueprint." xtra-path)))

  (missing-path-error [_ {:keys [pre-validation-transform original]} missing-path]
    (prefixed pre-validation-transform original
              (format "Map did not contain expected path %s." missing-path)))

  (predicate-fail-error [_ {:keys [full-path blueprint pre-validation-transform original]} val-at-path pred]
    (prefixed pre-validation-transform original
              (if (empty? full-path)
                (format "Value %s did not match predicate '%s'."
                        (pr-str val-at-path) (intelli-print blueprint pred))
                (format "Value %s, at path %s, did not match predicate '%s'."
                        (pr-str val-at-path) full-path (intelli-print blueprint pred)))))

  (instance-of-fail-error [_ {:keys [full-path blueprint pre-validation-transform original]} val-at-path expected-class]
    (prefixed pre-validation-transform original
              (if (empty? full-path)
                (format "Expected value %s to be an instance of class %s, but was %s"
                        (pr-str val-at-path) (pr-str expected-class) (pr-str (class val-at-path)))
                (format "Expected value %s, at path %s, to be an instance of class %s, but was %s"
                        (pr-str val-at-path) full-path (pr-str expected-class) (pr-str (class val-at-path))))))

  (pre-validation-transform-error [_ {:keys [full-path blueprint]} val-at-path pre-validation-transform-fn]
    (if (empty? full-path)
      (format "Value %s could not be transformed before validation using '%s'."
              (pr-str val-at-path) pre-validation-transform-fn)
      (format "Value %s, at path %s, could not be transformed before validation using '%s'."
              (pr-str val-at-path) full-path pre-validation-transform-fn))))

;; used to hold state of one `validation-errors` calculation
(def ^{:private true :dynamic true} *error-reporter* nil)
(def ^{:private true :dynamic true} *data-under-validation* nil)
(def ^{:private true :dynamic true} *data-under-validation---post-transformation* nil)
(def ^{:private true :dynamic true} *blueprint* nil)
(def ^{:private true :dynamic true} *parent-path* nil)
(def ^{:private true :dynamic true} *all-wildcard-paths* nil)
(def ^{:private true :dynamic true} *blueprint-without-wildcard-paths* nil)
(def ^{:private true :dynamic true} *pre-validation-transform* nil)
(def ^{:private true :dynamic true} *original* nil)

(defn- state-map-for-reporter [full-path]
  {:data-under-validation *data-under-validation*
   :blueprint *blueprint*
   :parent-path *parent-path*
   :full-path full-path
   :all-wildcard-paths *all-wildcard-paths*
   :blueprint-without-wildcard-paths *blueprint-without-wildcard-paths*
   :pre-validation-transform *pre-validation-transform*
   :original *original*})

(declare validation-errors valid?)

(defn- safe-keys [x]
  (when (map? x)
    (keys x)))

(defn- wildcard-path->concrete-paths [m [path-first & path-rest :as the-wildcard-path]]
  (if (empty? the-wildcard-path)
    [[]]
    (let [keys-that-match-blueprint (if (s/wildcard? path-first)
                                      (filter #(valid? (:blueprint path-first) %) (safe-keys m))
                                      [path-first])]
      (for [k-that-matches-blueprint keys-that-match-blueprint
            one-of-the-concrete-path-ends (wildcard-path->concrete-paths (get m k-that-matches-blueprint) path-rest)]
        (vec (cons k-that-matches-blueprint one-of-the-concrete-path-ends))))))

(defn- errors-for-concrete-path [blueprint-path blueprint]
  (let [val-at-path (get-in *data-under-validation---post-transformation* blueprint-path ::not-found)
        contains-path? (not= ::not-found val-at-path)
        full-path (into *parent-path* blueprint-path)]
    (cond (and (not contains-path?) (s/optional-path? blueprint-path))
          []

          (not contains-path?)
          [(missing-path-error *error-reporter* (state-map-for-reporter full-path) full-path)]

          :else
          (validation-errors *error-reporter* full-path blueprint val-at-path))))

(defn- errors-for-possibly-wildcard-path [blueprint-path blueprint]
  (if (s/wildcard-path? blueprint-path)
    (let [concrete-paths (wildcard-path->concrete-paths *data-under-validation---post-transformation* blueprint-path)
          ;; TODO ALex - Sep 15, 2012 - this is here because metadata lost - add abstraction to keep metadata for blueprints across a translation
          concrete-paths (if (s/optional-path? blueprint-path) (map s/optional-path concrete-paths) concrete-paths)]
      (mapcat #(errors-for-concrete-path % blueprint) concrete-paths))
    (errors-for-concrete-path blueprint-path blueprint)))


(defn- path-content-errors []
  (->> (s/blueprint-rows *blueprint*)
       (mapcat (fn [[blueprint-path blueprint]]
                 (errors-for-possibly-wildcard-path blueprint-path blueprint)))
       set))

(defn- shorten-to-blueprint-path-set
  "Since the result of calling 'paths' will be paths that go deeper into
   the map than our blueprint may specify, we truncate the result of calling
   'paths' to the longest version of them that is included in the blueprint's path set."
  [all-paths blueprint-path-set]
  (set (for [path all-paths]
         (if-let [validated-subpath (last (filter blueprint-path-set (u/subpaths path)))]
           validated-subpath
           path))))

(defn- remove-subpaths [paths]
  (let [all-subpaths (distinct (mapcat u/subpaths paths))
        any-of-all-subpaths-is-super-path? (fn [p]
                                             (some #(and (u/subpath? p %) (not= p %))
                                                   all-subpaths))]
    (remove any-of-all-subpaths-is-super-path? paths)))

(defn- extraneous-paths []
  (let [blueprint-paths (set (remove-subpaths (s/blueprint-path-set *blueprint-without-wildcard-paths*)))
        shortened (shorten-to-blueprint-path-set (u/paths *data-under-validation---post-transformation*) blueprint-paths)]
    (set/difference shortened blueprint-paths)))

(defn- covered-by-wildcard-path? [[path-first & path-rest :as path-to-check] [wildcard-first & wildcard-rest :as wildcard-path]]
  (let [path-to-check-count (count path-to-check)]
    (cond (not= path-to-check-count (count wildcard-path))
          false

          (zero? path-to-check-count)
          true

          (s/wildcard? wildcard-first)
          (if (valid? (:blueprint wildcard-first) path-first)
            (covered-by-wildcard-path? path-rest wildcard-rest)
            false)

          :else
          (covered-by-wildcard-path? path-rest wildcard-rest))))

(defn- matches-any-wildcard-path? [path]
  (some (partial covered-by-wildcard-path? path) *all-wildcard-paths*))

(defn- extraneous-paths-errors []
  (set (for [xtra-path (extraneous-paths)
             :when (not-any? matches-any-wildcard-path? (u/subpaths xtra-path))]
         (extraneous-path-error *error-reporter*
                                (state-map-for-reporter *parent-path*)
                                (into *parent-path* xtra-path)))))

(defn- constraint-errors []
  (set (for [c (:constraints *blueprint*)
             :when (not (valid? c *data-under-validation---post-transformation*))]
         (constraint-error *error-reporter* (state-map-for-reporter []) c))))

(defmacro ^{:private true} with-map-bindings [& body]
  `(binding [*all-wildcard-paths* (s/wildcard-path-set *blueprint*)
             *blueprint-without-wildcard-paths* (s/subtract-wildcard-paths *blueprint*)]
     ~@body))

(defn- map-loose-validation-errors [_parent-path_ _blueprint_ _m_]
  (with-map-bindings
    (path-content-errors)))

(defn- map-strict-validation-errors [_parent-path_ _blueprint_ _m_]
  (with-map-bindings
    (set/union (path-content-errors)
               (extraneous-paths-errors))))

(defn- coll-validation-errors [path-idx-fn parent-path blueprint xs]
  (let [blueprint (:blueprint-spec blueprint)]
    (set (mapcat (fn [idx x]
                   (validation-errors *error-reporter* (conj parent-path (path-idx-fn idx)) blueprint x))
                 (range)
                 xs))))

(def ^{:private true} set-validation-errors (partial coll-validation-errors (constantly :*)))
(def ^{:private true} seq-validation-errors (partial coll-validation-errors identity))

(defn- seq-layout-validation-errors [parent-path blueprint xs]
  (let [layout (:blueprint-spec blueprint)]
    (set (mapcat (fn [idx blueprint-x x]
                   (validation-errors *error-reporter* (conj parent-path idx) blueprint-x x))
                 (range)
                 layout
                 xs))))

(defn- class-validation-errors [parent-path blueprint x]
  (let [expected-class (:blueprint-spec blueprint)]
    (if-not (instance? expected-class x)
      #{(instance-of-fail-error *error-reporter* (state-map-for-reporter parent-path) x expected-class)}
      #{})))

(defn- or-statement-validation-errors [parent-path blueprint x]
  (let [blueprints (:blueprint-spec blueprint)
        error-msg-batches (map #(validation-errors *error-reporter* parent-path % x) blueprints)
        error-msgs        (set (apply concat error-msg-batches))]
    (if-not (< (count (remove empty? error-msg-batches))
               (count blueprints))
      error-msgs
      #{})))

(defn- and-statement-validation-errors [parent-path blueprint x]
  (let [blueprints (:blueprint-spec blueprint)]
    (set (mapcat #(validation-errors *error-reporter* parent-path % x) blueprints))))

(defn- predicate-validation-errors [parent-path blueprint x]
  (let [pred (:blueprint-spec blueprint)]
    (if-not ((u/fn->fn-thats-false-if-throws pred) x)
      #{(predicate-fail-error *error-reporter* (state-map-for-reporter parent-path) x pred)}
      #{})))

(defn- validation-fn [blueprint]
  (case [(:type blueprint) (boolean (:strict blueprint))]
    [:map false]           map-loose-validation-errors
    [:map true]            map-strict-validation-errors
    [:seq false]           seq-validation-errors
    [:seq-layout false]    seq-layout-validation-errors
    [:set false]           set-validation-errors
    [:class false]         class-validation-errors
    [:or-statement false]  or-statement-validation-errors
    [:and-statement false] and-statement-validation-errors
    [:predicate false]     predicate-validation-errors))

(defn- prepare-for-validation [blueprint x]
  (try
    (if-let [transform (:pre-validation-transform blueprint)]
      (transform x)
      x)
    (catch Exception _
      ::exception)))

(defn validation-errors
  "Returns a set of all the validation errors found when comparing a given
   item x, against the supplied blueprint.
   If blueprint is not already a blueprint, it will attempt to make a simple blueprint from it."
  ([blueprint x]
     (validation-errors (StringErrorReporter.) [] blueprint x))
  ([error-reporter blueprint x]
     (validation-errors error-reporter [] blueprint x))
  ([error-reporter parent-path blueprint x]
     (let [blueprint (s/ensure-blueprint blueprint)
           prepped-x (prepare-for-validation blueprint x)]
       (binding [*error-reporter* error-reporter
                 *data-under-validation* x
                 *pre-validation-transform* (or *pre-validation-transform* (:pre-validation-transform blueprint))
                 *original* (or *original* (if (:pre-validation-transform blueprint) x nil))
                 *data-under-validation---post-transformation* prepped-x
                 *blueprint* blueprint
                 *parent-path* parent-path]
         (if (= ::exception prepped-x)
           #{(pre-validation-transform-error *error-reporter* (state-map-for-reporter *parent-path*) x (:pre-validation-transform *blueprint*))}
           (if-let [c-errors (seq (constraint-errors))]
             (set c-errors)
             ((validation-fn blueprint) parent-path blueprint prepped-x)))))))

(defn valid?
  "Returns true if calling `validation-errors` would return no errors"
  [blueprint m]
  (empty? (validation-errors (StringErrorReporter.) blueprint m)))

(defn validate-and-handle
  "Validates item x against a blueprint.
   If it passes, then calls success-handler-fn passing m to it.
   If it fails, then calls error-handler-fn passing m and any validation errors to it."
  ([error-reporter m blueprint success-handler-fn error-handler-fn]
     (if-let [errors (seq (validation-errors error-reporter blueprint m))]
       (error-handler-fn m errors)
       (success-handler-fn m)))
  ([m blueprint success-handler-fn error-handler-fn]
     (validate-and-handle (StringErrorReporter.) m blueprint success-handler-fn error-handler-fn)))