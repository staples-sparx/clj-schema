To Use
======

```clj
[org.clojars.runa/clj-schema "0.7.2"]
```

Schemas for Clojure Maps
========================

*   [Getting Started](https://github.com/runa-dev/clj-schema/wiki/Getting-Started)

Define validation schemas for validating maps.

Schemas are any number of paths through a nested map, paired with a
validator.

There are 5 types of validators: 

*   any predicate function 
*   any `Class` object, i.e. `String`, `clojure.lang.Keyword`, `java.util.Date`, etc 
*   any clj-schema schema 
*   `[validator1 validator2]` to indicate both validator1 AND validator2 
*   `[:or validator1 validator2]` to indicate both validator1 OR validator2

Any validator may be wrapped in sequence-of to indicate the value should
be sequential, or wrapped in set-of to indicate the value is a set. By
default, schemas assume the value at a path to be a singular value.

A path may be marked as an `optional-path`. This means that is doesn't
have to be present, but if it is, it must match the given validator.

Wildcard paths are paths where one or more peices are defined as anything
matching a given validator.

Example Schema:

```clj
(:require [clj-schema.schema :refer [def-map-schema optional-path sequence-of 
                                     map-schema set-of]])

(def-map-schema bar-schema 
  [[:a :b :c] pred 
   [:x :y :z] [pred2 pred3 z-schema] ;; implicit 'and' - all three must pass 
   [:p :q :r] [:or nil? r-schema] ;; an 'or' statement - need just one to pass 
   (optional-path [:z]) (sequence-of string?) 
   [:a b :d] (map-schema :loose [[:cat :name] String ;; can use Java Class objects directly 
   [:cat :colors] (set-of String)])]
```

Example schema w/ wildcard paths:

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

You can combine more than one schema into a combined schema like this:

```clj
(def-map-schema bar-schema
  foo-schema
  [[:bar] String
   [:baz] #(re-matches #".*baz.*" %)])
```

Schemas are just maps:

```clj
(def-map-schema foo-schema [[:a] String])
;; foo-schema
;; => {:type :map
;;     :schema-spec [[:a] java.lang.String]
;;     :constraints ({:predicate #<schema$fn__145 clj_schema.schema$fn__145@12948069>
;;                    :source (fn [m] (or (nil? m) (map? m)))}) 
;;     :strict true}
```

As you can see in the example above, `def-map-schema` creates a strict schema by default, which expects only the paths it describes to be present on the given map.

`(def-map-schema :loose [[:a] String])` creates a loose schema, which expects its paths to be
present but does not complain about extra paths

Map schemas can be altered to be loose or strict using `as-loose-schema` or `as-strict-schema`


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
```

```clj
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
(def-schema rectangle-schema
  [[:height] Number 
   [:width]  Number])

(def-fixture-factory rectangle rectangle-schema
  ([h w] 
    {:height h :width w}) 
  ([length]
    {:height length :width length}))
```

Seq and Set Validation and Introducing Constraints
==================================================

You can also add constraints: predicates that apply to the entire 
data structure under validation:

```clj
(def-map-schema :loose sorted-unique
  (constraints sorted? (fn [m] (= (count (keys m)) 
                                  (count (distinct (keys m))))))
  [[:id] String])

;; A checkerboard schema, describing an 8x8 seq of seqs, 
;; that contains only 1's and 0's.
(def black-square #(= 0 %))
(def white-square #(= 1 %))

(def-seq-schema checkers-row
  (constraints (fn [row] (= 8 (count row))))
  [:or white-square black-square])  

(def-seq-schema checkers-board
  (constraints (fn [row] (= 8 (count row))))
  checkers-row-schema)

;; and for all your marble-based apps:
(def-set-schema bag-of-marbles
  (constraints #(> 50 (count %)))
  (OneOf :red :blue :green :yellow :striped :polka-dot :black :white))  
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
          [clj-schema.validation :as val]
          [clj-schema.validators :as v])

(def-map-schema user-params-schema 
  [[:name] v/NonEmptyString])

(checked/POST "/user" {^{:check #(val/validation-errors user-params-schema %)} params :params}
  (do-something params))
```

Developer Tests
===============

Run them with `./run-tests.sh`.  This will run all unit tests in Clojure 1.2 - 1.5, ensuring this library is usable by the widest number of projects.

Contributors
============
Alex Baranosky
Laurent Petit
Punit Rathore

License
=======

Licensed under the [MIT license](http://mit-license.org/).

