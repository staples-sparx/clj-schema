Schemas for Clojure maps
========================

Define validation schemas for validating maps.

Schemas are any number of paths through a nested map, paired with a
validator.

There are 5 types of validators: 

*   any predicate function * any Class object, i.e. String, clojure.lang.Keyword, java.util.Date, etc 
*   any clj-schema schema 
*   [validator1 validator2] to indicate both validator1 AND validator2 
*   [:or validator1 validator2] to indicate both validator1 OR validator2

Any validator may be wrapped in sequence-of to indicate the value should
be sequential, or wrapped in set-of to indicate the value is a set. By
default, schemas assume the value at a path to be a singular value.

A path may be marked as an `optional-path`. This means that is doesn't
have to be present, but if it is, it must match the given validator.

Wildcard paths are paths where one or more peices are defined as anything
matching a given validator.

Example Schema:

```clj
(defschema bar-schema 
  [[:a :b :c] pred 
   [:x :y :z] [pred2 pred3 z-schema] ;; implicit 'and' - all three must pass 
   [:p :q :r] [:or nil? r-schema] ;; an 'or' statement - need just one to pass 
   (optional-path [:z]) (sequence-of string?) 
   [:a b :d] (loose-valdiation-schema [[:cat :name] String ;; can use Java Class objects directly 
   [:cat :colors] (set-of String)])]
```

Example schema w/ wildcard paths:

```clj
(defschema foo-schema 
  [[:a (wild String) (wild Number)] String])
```

  => matches maps such as: 
    `{:a {\"car\" {1.21 \"jigawatts\"}}}` 
    `{:a {\"banana\" {2 \"green\"}}}`

`defschema` creates a strict schema, which expects only the paths it
describes to be present on the given map.

`def-loose-schema` creates a loose schema, which expects its paths to be
present but does not complain about extra paths


Map Validation Using Schemas
============================
...


Type Coercion using Schemas
===========================
...


Developer Tests
===============

Run them with `./run-tests.sh`.  This will run all unit tests in Clojure 1.2 - 1.5, ensuring this library is usable by the widest number of projects.