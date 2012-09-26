(ns clj-schema.coerce-test
  (:use clojure.test
        clj-schema.coerce)
  (:require [clj-schema.fixtures :as fxs]
            [clj-schema.schema :as sch])
  (:import clojure.lang.Keyword))

(sch/defschema car-schema
  [[:color] Keyword])

(deftest test-coerce-class
  (are [schema input-map result] (= result (coerce-map (sch/strict-schema schema) input-map))

       ;; simple case
       [[:a] String]
       {}
       {}

       ;; coerces map value to the expected Class
       [[:a] String
        [:b] Keyword]
       {:a 12840 :b 123}
       {:a "12840" :b :123}

       ;; coerces sub-maps that are covered by sub-schemas 
       [[:car] car-schema]
       {:car {:color "green"}}
       {:car {:color :green}}

       ;; coerces submaps within vectors 
       [[:cars] (sch/sequence-of car-schema)]
       {:cars [{:color "green"}
               {:color "red"}]}
       {:cars [{:color :green}
               {:color :red}]}

       ;; un-coercable/un-supported classes pass through untouched
       [[:a] Integer]
       {:a "string"}
       {:a "string"}

       ;; if the validator is an AND/OR statement, takes the first Class found
       [[:a]  [String (complement empty?) [:or Integer Long]]] ;; yes this makes no sense
       {:a :roberto}                                 ;; just illustrating that is using the first Class
       {:a "roberto"}

       ;; if no Class in AND/OR statement then no coercion happens
       [[:a]  [pos? number?]]
       {:a :roberto}         
       {:a :roberto}

       ;; nils always coerce to nil
       [[:a :b] String
        [:a :c] Keyword]
       {:a {:b nil :c nil}}
       {:a {:b nil :c nil}}

       ;; when an AND/OR statement has a schema in it use the schema first
       [[:car] [nil? car-schema]]
       {:car {:color "green"}}
       {:car {:color :green}}

       ;; ... <continued>
       [[:car] [:or nil? car-schema]]
       {:car {:color "green"}}
       {:car {:color :green}}

       ;; if something is already the desired type, then no coercion happens
       [[:a] String]
       {:a "a"}
       {:a "a"}

       ;; ... <continued>
       [[:a] Keyword]
       {:a :b}
       {:a :b}

       ;; finds Classes nested inside multiple AND/ORs
       [[:a] [:or nil? [Keyword #{:x :y :z}]]]
       {:a "x"}
       {:a :x}))

(deftest test-attempt-to-coerce-wildcard-schema
  (is (thrown-with-msg? IllegalArgumentException
        #"Does not support type coercion for schemas with wildcards"
        (coerce-map (sch/strict-schema [[(sch/wild :a)] true?]) {:any "map"}))))
