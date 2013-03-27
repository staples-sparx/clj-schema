To Use
======

```clj
[org.clojars.runa/clj-schema "0.8.10"]
```

Travis CI Status
================

[![Build Status](https://travis-ci.org/runa-dev/clj-schema.png)](https://travis-ci.org/runa-dev/clj-schema)

Schemas for Clojure Data Structures and Values
==============================================

*   [Getting Started](https://github.com/runa-dev/clj-schema/wiki/Getting-Started)
*   [API Docs](http://runa-dev.github.com/clj-schema/)
*   [Google Group](https://groups.google.com/forum/?fromgroups#!forum/clj-schema)


Schemas
=======

There are three main types of schemas: `def-map-schema`, `def-seq-schema`, `def-set-schema`.  These 
three all come with the built-in assumption that the target of the schema matches 
the expected type, or is nil.

There is also a category of schemas that can be applied to any type. We call these
simple-schemas.

If you don't create a simple-schema explicitly then in most cases 
clj-schema will implicitly create one for you:

`(validation-errors String "A")` is equivalent to `(validation-errors (simple-schema String) "A")`
`(validation-errors number? 99)` is equivalent to `(validation-errors (simple-schema number?) 99)`
`(validation-errors [number? pos?] 77)` is equivalent to `(validation-errors (simple-schema [number? pos?]) 77)`
`(validation-errors [:or String Keyword] :a)` is... 
  equivalent to `(validation-errors (simple-schema [:or String Keyword]) :a)`


Map Schemas
===========

Map schemas are defined with any number of paths through a nested map, paired
with another schema to check it.  The schemas to check it, can be any of:
map-schema, seq-schema, set-schema, simple-schemas, or something that can 
be converted to a simple-schema on the fly.

Any schema may be wrapped in `sequence-of` to indicate the value should
be sequential, or wrapped in `set-of` to indicate the value is a set.  These
are aliases for `seq-schema` and `set-schema`

A path may be marked as an `optional-path`. This means that is doesn't
have to be present, but if it is, it must match the given schema.

Wildcard paths are paths where one or more peices are defined as anything
matching a given schema.

Example Map Schema:

```clj
(:require [clj-schema.schema :refer [def-map-schema optional-path sequence-of 
                                     map-schema set-of]])

;; assuming we've already defined a z-schema and an r-schema
(def-map-schema bar-schema 
  [[:type] :bar
   [:a :b :c] pred 
   [:a :b :d] [:or -1 0 1 2 3 4]
   [:x :y :z] [pred2 pred3 z-schema] ;; implicit 'and' - all three must pass 
   [:p :q :r] [:or nil? r-schema] ;; an 'or' statement - need just one to pass 
   (optional-path [:z]) (sequence-of string?) 
   [:a b :d] (map-schema :loose [[:cat :name] String]) ;; can use Java Class objects directly 
   [:cat :colors] (set-of String)])]
   ```

Example map schema w/ wildcard paths:

```clj
(def-map-schema foo-schema 
  [[:a (wild String) (wild Number)] String])

;; matches maps such as: 
{:a {"car" {1.21 "jigawatts"}}}    

;; and:
{:a {"banana" {2 "green"
               5 "yellow"}
      "apple" {1 "rotten"
               4 "delicious"}}}

;; and:
{:a {}}
```

You can combine more than one map schema into a combined schema, that will simply check all
paths (and constraints) from all included schemas. 

```clj
(def-map-schema bar-schema
  foo-schema
  [[:bar] String
   [:baz] #"baz"])
```

All schemas are just maps:

```clj
(def-map-schema foo-schema [[:a] String])
;; foo-schema
;; => {:type :map 
;;     :schema-spec [[:a] {:type :class, :schema-spec java.lang.String, :constraints []}]
;;     :constraints [{:source [:or nil? map?]
;;                    :type :or-statement
;;                    :schema-spec [#<core$nil_QMARK_ clojure.core$nil_QMARK_@340ae1cf> 
;;                                  #<core$map_QMARK_ clojure.core$map_QMARK_@366ef7ba>] 
;;                    :constraints []}]
;;     :strict true}
```

`def-map-schema` creates a strict schema by default, which expects only the paths it describes to be present on the given map.

`(def-map-schema :loose my-schema [[:a] String])` creates a loose schema, which expects its paths to be
present but does not complain about extra paths.


Seq/Set Validation and Introducing Constraints
==================================================

You can also add constraints: schemas, or simple schema precursors that apply to the entire
data structure under validation:

```clj
(def-map-schema :loose sorted-unique
  (constraints sorted? (fn [m] (= (count (keys m))
                                  (count (distinct (keys m))))))
  [[:id] String])
```

#### A checkerboard schema, describing an 8x8 seq of seqs, that contains only 1's and 0's.

```clj
(def-seq-schema checkers-row
  (constraints (fn [row] (= 8 (count row))))
  [:or 0 1])

(def-seq-schema checkers-board
  (constraints (fn [row] (= 8 (count row))))
  checkers-row-schema)
```        

#### An alternate, layout-based checkerboard schema, ensures checkering of 1's and 0's:

```clj
(def-seq-schema :layout white-row
  [0 1 0 1 0 1 0 1])

(def-seq-schema :layout black-row
  [1 0 1 0 1 0 1 0])

(def-seq-schema :layout checkers-board-schema
  [white-row black-row white-row black-row white-row black-row white-row black-row])
```

#### Sets schemas:
```clj
(def-set-schema possible-states
  (constraints #(> 50 (count %)))
  #(re-matches #"state\d+" %)) 
```


Map Validation Using Schemas
============================
 
`validation-errors` can be used to test a map vs a given schema. It can 
find a variety of issues:

*   thing we're validating wasn't a map
*   map contained a path not specified in the schema (only for strict schemas)
*   map was missing a specified path
*   a path's value was single, but was specified as sequential
*   a path's value was single, but was specified as a set
*   a path's value was sequential, but was specified as single
*   a path's value was a set, but was specified as single
*   a path's value didn't pass the given predicate
*   a path's value's Class wasn't an instance of the specified Class

```clj
(def-map-schema person-schema
  [[:name :first] String
   [:name :last]  String
   [:height]      Double])
```

```clj
(:require [clj-schema.validation :refer [validation-errors])

(validation-errors person-schema {})
;; => #{"Map did not contain expected path [:name :first]." 
;;      "Map did not contain expected path [:name :last]."
;;      "Map did not contain expected path [:height]."}
```

Supports alternate report formats. You just have to implement the ErrorReporter 
protocol, and then pass it in like this:

```clj
(:require [clj-schema.validation :refer [ErrorReporter validation-errors]])

;; An example of a data-based error format, instead of using the default StringErrorReporter.

(deftype CljDataErrorReporter
  []
  ErrorReporter
  ;; The second arg to each of the protocol's methods is a map of various state 
  ;; of the validation at the time the error was generated. Your protocol 
  ;; implementations can access any of that information for your reporting purposes.
  ;; For reference see `clj-schema.validation/state-map-for-reporter` which creates that map.
  (constraint-error [_ {} constraint]
    {:type :constraint-error
     :data data-under-validation
     :constraint constraint}) 

  (extraneous-path-error [_ {:keys [data-under-validation]} extra-path]
    {:type :extraneous-path
     :data data-under-validation
     :unexpected-path extra-path})

  (missing-path-error [_ {:keys [data-under-validation]} missing-path]
    {:type :missing-path
     :data data-under-validation
     :missing-path missing-path})

   (predicate-fail-error [_ {:keys [full-path data-under-validation]} val-at-path pred]
    {:type :predicate
     :data data-under-validation
     :path full-path
     :value val-at-path
     :predicate pred})

  (instance-of-fail-error [_ {:keys [full-path data-under-validation]} val-at-path expected-class]
    {:type :instance-of
     :value val-at-path
     :data data-under-validation
     :path full-path
     :values-class (class val-at-path)
     :expected-class expected-class}))

(validation-errors CljDataErrorReporter person-schema {:map "you" 
                                                       :are "validating"})
```


Test Data and Test Data Factories
=================================

Rationale: to keep your test data from growing out of sync with your real data,
and to make factories for concisely creating valid fake data for tests.

You can find this code in the clj-schema.fixtures namespace.

These should probably be renamed to something less confusing. Any ideas?

```clj
(:require [clj-schema.fixtures :refer [def-fixture def-fixture-factory])

(def-map-schema person-schema
  [[:name :first] String
   [:name :last]  String
   [:height]      Double])

;; This will blow up at load time if the 'alex' map you want to use in your test
;; turns out to not be valid
(def-fixture alex person-schema
  {:name {:first "Alex"
          :last "Baranosky"}
   :height 71})

;; Let's define a factory that makes people. And then...
(def-fixture-factory person person-schema
  [& {:keys [first-name last-name height]
      :or {first-name "Alex"
           last-name "Baranosky"
           height 71}}]
  {:name {:first first-name
          :last last-name}
   :height height})

;; ... write a test that tests a fictional function called sort-people-by-height
(deftest test-sort-people-by-height
  (is (= [(person :height 67) (person :height 89) (person :height 98)]
         (sort-people-by-height [(person :height 98) (person :height 67) (person :height 89)]))))

;; Fixture factories can also be defined as multi-arity
(def-seq-schema :layout point-schema
  (constraints (fn [[x y]] 
                 (= (class x) (class y))))
  [Number Number])

(def-fixture-factory point point-schema
  ([x y] 
    [x y])
  ([length]
    [length length]))
```


Validated Compojure Routes
==========================

Greg Spurrier beat me to it and created a nice library for creating validated 
routes, called [checked-route](https://github.com/gregspurrier/checked-route). It is a natural match to clj-schema, and I 
recommend you check it out.

```clj
;; Example:
(:require [checked-route.route :as checked]
          [clj-schema.schema :as sch]
          [clj-schema.simple-schemas :as ss]
          [clj-schema.validation :as val])
          
(def-map-schema user-params-schema
  [[:name] ss/NonEmptyString])

(checked/POST "/user" {^{:check #(val/validation-errors user-params-schema %)} params :params}
  (do-something params))
```

Developer Tests
===============

Run them with `./run-tests.sh`.  This will run all unit tests in Clojure 1.2 - 1.5, ensuring this library is usable by the widest number of projects.

Contributors
============
*   Alex Baranosky
*   Laurent Petit
*   Punit Rathore

License
=======

Licensed under the [MIT license](http://mit-license.org/).

