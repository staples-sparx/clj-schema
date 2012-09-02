(ns clj-schema.schema-test
  (:use clojure.test
        clj-schema.schema)
  (:import clojure.lang.Keyword))


(def-validation-schema name-schema   [[:name :first] string?])
(def-validation-schema height-schema [[:height] integer?])
(def-validation-schema count-schema [[:count] Integer])
(def-validation-schema product-schema [[:quantity] Integer
                                       [:price]    Integer])
(def-loose-validation-schema loose-height-schema [[:height] integer?])
(def-validation-schema person-schema
  name-schema
  height-schema)

(def-loose-validation-schema loose-person-schema
  [[:name :first] string?
   [:height] integer?])

(def-validation-schema family-schema
  [[:mom] person-schema
   [:dad] person-schema])

(def-validation-schema mom-strict-dad-loose-family-schema
  [[:mom] person-schema
   [:dad] loose-person-schema])

(defn first-name-bob? [x]
  (-> x :name :first (= "Bob")))

(deftest test-validation-errors
  (are [schema m errors] (= errors (validation-errors schema m))

    ;;;; Degenerate cases

    nil             {}          #{}
    []              {}          #{}
    nil             nil         #{}
    []              nil         #{}

    family-schema [1 2 3] #{"At path [], expected a map, got [1 2 3] instead."}

    ;;

    ;;;; One predicate per path

    [[:a] number?]
    {:a 1}
      #{}

    [[:b] number?]
    {}
      #{"Map did not contain expected path [:b]."}

    [[:bb] number?]
    {:bb "bb"}
      #{"Map value \"bb\", at path [:bb], did not match predicate 'number?'."}

    [[:b :c] number?]
    {:b "a"}
      #{"Path [:b] was not specified in the schema."
      "Map did not contain expected path [:b :c]."}

    [[:b :c] number?]
    {:b {:c "a"}}
      #{"Map value \"a\", at path [:b :c], did not match predicate 'number?'."}

    ;;

    ;;;; Multiple predicates per path - lists all failures, not just first

    [[:a] [number? pos?]]
    {:a 1}
      #{}

    [[:b] [number? pos?]]
    {}
      #{"Map did not contain expected path [:b]."}

    [[:b] [number? pos?]]
    {:b "a"}
      #{"Map value \"a\", at path [:b], did not match predicate 'number?'."
      "Map value \"a\", at path [:b], did not match predicate 'pos?'."}

    [[:b :c] [number? pos?]]
    {:b "a"}
      #{"Path [:b] was not specified in the schema."
      "Map did not contain expected path [:b :c]."}

    [[:b :c] [number? pos?]]
    {:b {:c "a"}}
      #{"Map value \"a\", at path [:b :c], did not match predicate 'number?'."
      "Map value \"a\", at path [:b :c], did not match predicate 'pos?'."}

    ;;

    ;;;; Handles combo of missing paths and erroneous values
    [[:b :c] [number? pos?]
     [:b :d] identity
     [:x :y] [neg? number?]
     [:z :z :top] keyword?]
    {:b {:c "a" :d :foo}
     :x {:y 99}}
      #{"Map value \"a\", at path [:b :c], did not match predicate 'number?'."
      "Map value \"a\", at path [:b :c], did not match predicate 'pos?'."
      "Map did not contain expected path [:z :z :top]."
      "Map value 99, at path [:x :y], did not match predicate 'neg?'." }

    ;;;; when using 'sequence-of' validator is applied against each element in the sequence at that key
    [[:a] (sequence-of person-schema)]
    {:a [{:name {:first "Roberto"} :height 11} {:name {:first "Roberto"} :height "11"}]}
      #{"Map value \"11\", at path [:a :height], did not match predicate 'integer?'."}

    ;;;; if the schema path isn't present, and using a schema validator the seq, we get a "not present" error
    [[:a] (sequence-of person-schema)]
    {:not-a [{:name {:first "Roberto"} :height 34} {:name {:first "Roberto"} :height "34"}]}
      #{"Map did not contain expected path [:a]."
      "Path [:not-a] was not specified in the schema."}

    ;;;; if an optional schema path isn't present - no error messages
    [(optional-path [:a]) (sequence-of person-schema)]
    {:not-a [{:name {:first "Roberto"} :height 91} {:name {:first "Roberto"} :height "91"}]}
      #{"Path [:not-a] was not specified in the schema."}

    ;;;; `optional-path` has no effect if the schema path is present
    [(optional-path [:a]) (sequence-of person-schema)]
    {:a [{:name {:first "Roberto"} :height 70} {:name {:first "Roberto"} :height "70"}]}
      #{"Map value \"70\", at path [:a :height], did not match predicate 'integer?'."}

    ;;;; you can mix types of validators: preds with schemas
    [[:a] (sequence-of [first-name-bob? person-schema])]
    {:a [{:name {:first "Roberto"} :height 44} {:name {:first "Chris"} :height "44"}]}
      #{"Map value \"44\", at path [:a :height], did not match predicate 'integer?'."
      "Map value {:name {:first \"Roberto\"}, :height 44}, at path [:a], did not match predicate 'furtive.schemas.validation-schema-spec/first-name-bob?'."
      "Map value {:name {:first \"Chris\"}, :height \"44\"}, at path [:a], did not match predicate 'furtive.schemas.validation-schema-spec/first-name-bob?'."}

    ;;;; ...  multiple strict schemas together makes little sense - one schema will think extra keys were not specified by it, though they were by the other schema
    [[:a] (sequence-of [name-schema person-schema])]
    {:a [{:name {:first :Roberto} :height 69} {:name {:first "Roberto"} :height "69"}]}
      #{"Map value \"69\", at path [:a :height], did not match predicate 'integer?'."
      "Map value :Roberto, at path [:a :name :first], did not match predicate 'string?'."
      "Path [:a :height] was not specified in the schema."}

    ;;;; validator on right of schema, can be made an or, and can work with both preds and schemas mixed
    [[:a] [:or nil? person-schema]]
    {:a nil}
      #{}

    ;;;; you can have just one thing in the ':or' - but please don't it is weird
    [[:a] (sequence-of [:or person-schema])]
    {:a [{:name {:first "Roberto"} :height "76"}]}
      #{"Map value \"76\", at path [:a :height], did not match predicate 'integer?'."}

    ;;;; when both :or options fail - see errors for both 'nil?' and 'person-schema'
    [[:a] [:or nil? person-schema]]
    {:a {:name {:first "Roberto"} :height "66"}}
      #{"Map value {:name {:first \"Roberto\"}, :height \"66\"}, at path [:a], did not match predicate 'nil?'."
      "Map value \"66\", at path [:a :height], did not match predicate 'integer?'."}

    ;;;; or collects all failures in the sequence being checked
    [[:a] (sequence-of [:or nil? person-schema])]
    {:a [{:name {:first "Roberto"} :height "88"} {:name {:first "Roberto"} :height 88}]}
      #{"Map value {:name {:first \"Roberto\"}, :height \"88\"}, at path [:a], did not match predicate 'nil?'."
      "Map value \"88\", at path [:a :height], did not match predicate 'integer?'."}

    ;;;; nested schemas - no errors
    [[:a :family] family-schema]
    {:a {:family {:mom {:name {:first "Theresa"}
                        :height 42}
                  :dad {:name {:first "Stanley"}
                        :height 53}}}}
      #{}

    ;;;; nested schemas - with errors
    [[:a :family] family-schema]
    {:a {:family {:mom {:name {:first :Theresa}
                        :height 42}
                  :dad {:name {:first "Stanley"}
                        :height :53}}}}
     #{"Map value :53, at path [:a :family :dad :height], did not match predicate 'integer?'."
       "Map value :Theresa, at path [:a :family :mom :name :first], did not match predicate 'string?'."}

     ;;;; strict schemas fail if there are more keys than specified
     [[:a :family] family-schema]
     {:a {:family {:mom {:name {:first "Theresa"
                                :last "Greepostalla"}
                         :height 42
                         :favorite-book "Twilight"}
                   :child "David"
                   :dad {:name {:first "Stanley"
                                :middle "Roberto-Gustav"}
                         :height 53
                         :favorite-sport "Fishing"}}
          :house "Large"}
      :b {:car "Honda Accord"}}
       #{"Path [:b :car] was not specified in the schema."
       "Path [:a :family :mom :favorite-book] was not specified in the schema."
       "Path [:a :family :dad :name :middle] was not specified in the schema."
       "Path [:a :family :child] was not specified in the schema."
       "Path [:a :house] was not specified in the schema."
       "Path [:a :family :mom :name :last] was not specified in the schema."
       "Path [:a :family :dad :favorite-sport] was not specified in the schema."}

     ;;;; nested loose schemas don't cause extra path errors for their paths, even if inside a surrounding strict schema
     [[:a :family] mom-strict-dad-loose-family-schema]
     {:a {:family {:mom {:name {:first "Theresa"
                                :last "Greepostalla"}
                         :height 42
                         :favorite-book "Twilight"}
                   :child "David"
                   :dad {:name {:first "Stanley"
                                :middle "Roberto-Gustav"}
                         :height 53
                         :favorite-sport "Fishing"}} ;; Dad's loose so this extra key causes no problems
          :house "Large"}
      :b {:car "Honda Accord"}}
     #{"Path [:b :car] was not specified in the schema."
       "Path [:a :family :mom :favorite-book] was not specified in the schema."
       "Path [:a :family :child] was not specified in the schema."
       "Path [:a :house] was not specified in the schema."
       "Path [:a :family :mom :name :last] was not specified in the schema."}

     ;;;; stops looking for extra paths at validated path ends
     [[:a :family] map?]
     {:a {:family {:mom {:name {:first "Theresa"
                                :last "Greepostalla"}
                         :height 42
                         :favorite-book "Twilight"}
                   :child "David"
                   :dad {:name {:first "Stanley"
                                :middle "Roberto-Gustav"}
                         :height 53
                         :favorite-sport "Fishing"}} ;; Dad's loose so this extra key causes no problems
          :house "Large"}
      :b {:car "Honda Accord"}}
       #{"Path [:b :car] was not specified in the schema."
       "Path [:a :house] was not specified in the schema."}

     ;;;; validator *not* marked as 'sequence-of' but value is sequential - causes an error
     [[:a] person-schema]
     {:a [{:name {:first "Roberto"} :height "76"}]}
     #{"Map value [{:name {:first \"Roberto\"}, :height \"76\"}], at path [:a], was sequential but not tagged with 'sequence-of'."}

     ;;;; marked as 'sequence-of' but only one value - causes an error
     [[:a] (sequence-of person-schema)]
     {:a {:name {:first "Roberto"} :height "76"}}
       #{"Map value {:name {:first \"Roberto\"}, :height \"76\"}, at path [:a], was a single value but was tagged with 'sequence-of'."}

     ;;;; validator *not* marked as 'sequence-of' but value is sequential - causes an error
     [[:a] integer?]
     {:a [1 2 3]}
     #{"Map value [1 2 3], at path [:a], was sequential but not tagged with 'sequence-of'."}

     ;;;; using 'sequence-of' with an 'integer?' predicate - means there is a seq of integers
     [[:a] (sequence-of integer?)]
     {:a 1}
       #{"Map value 1, at path [:a], was a single value but was tagged with 'sequence-of'."}

     ;;;; nil is an acceptable value for a 'sequence-of' validator
     [[:a] (sequence-of integer?)]
     {:a nil}
     #{}

     ;;;; sequence-of can be used from within other nested validators -- [:a] is single item
     [[:a] [:or Number (sequence-of Number)]]
     {:a 4}
       #{}

     ;;;; ... <continued from above> -- [:a] is sequential
     [[:a] [:or Number (sequence-of Number)]]
     {:a [4 5 6 7]}
     #{}

     ;;;; nested loose schemas don't count toward strict schema's keys
     (strict-validation-schema
       [[:a] loose-height-schema])
     {:a {:height 72 :extra-key-doesnt-cuase-error "foo"}
      :b "oops"}
       #{"Path [:b] was not specified in the schema."}

     ;;;; can use Classes as a validator
     [[:a] String]
     {:a "Roberto"}
     #{}

     [[:a] String]
     {:a :Roberto}
       #{"Map value :Roberto at path [:a] expected class java.lang.String, but was clojure.lang.Keyword"}

     ;; instance-of? satisfies the predicate -- Long is an instance of Number
     [[:a] Number]
     {:a (long 999)}
     #{}

     ;;;; Wildcard paths

     ;; no problems if all paths match the wildcards in the path
     [[(wild Keyword) (wild string?) 99] String]
     {:a      {"b"      {99 "letter b"}}
      :xavier {"yellow" {99 "zebra"}}}
       #{}

     ;; validates the value at the given path, like normal
     [[:a (wild keyword?) (wild string?)] String]
     {:a {:x {"b" :b "c" "letter c"}}}
     #{"Map value :b at path [:a :x \"b\"] expected class java.lang.String, but was clojure.lang.Keyword"}

     ;; if a path exists that doesn't match the wildcard, it is considered an extraneous path
     [[:a] (strict-validation-schema [[(wild Keyword)] String])]
     {:a {"b" "foo" "c" "bar"}}
       #{"Path [:a \"c\"] was not specified in the schema."
       "Path [:a \"b\"] was not specified in the schema."}

     ;; can use Class objects as wildcard part of wildcard path
     [[:a (wild String)] String]
     {:a {"b" :b "c" "letter c"}}
     #{"Map value :b at path [:a \"b\"] expected class java.lang.String, but was clojure.lang.Keyword"}

     ;; can use 'and statements' in validators
     [[:a (wild [String #{"baz" "qux"}])] String]
     {:a {"baz" :b "c" "letter c"}}
       #{"Map value :b at path [:a \"baz\"] expected class java.lang.String, but was clojure.lang.Keyword"
       "Path [:a \"c\"] was not specified in the schema."}

     ;; if no keys of the leaf-map match the wildcard-validator, that is OK
     [[:a (wild string?)] String]
     {:a {999 :boom}}
     #{"Path [:a 999] was not specified in the schema."}

     ;; Wildcard paths match empty maps
     [[:a (wild string?)] String]
     {:a {}}
       #{}

     ;; extra paths on wild card paths are ok if the schema is loose
     (loose-validation-schema [[:a (wild string?)] String])
     {:a {999 :boom}}
     #{}

     ;; don't get missing path errors for nested schemas within wildcard paths - regression test Jun 15, 2012
     [[:carted (wild String)] product-schema]
     {:carted {"Sneetch" {:quantity 5 :price 100}}}
       #{}

     ;; doesn't confuse paths with string keys as wildcard paths - regression test Jun 15, 2012
     [["Sneetch" :unit-price-cents] string?]
     {"Sneetch" {:unit-price-cents "a"}}
     #{}

     ;; won't confuse them in nested paths either
     [["Sneetch" :unit-price-cents] (strict-validation-schema [[:a] string?])]
     {"Sneetch" {:unit-price-cents {:a "a"}}}
       #{}

     ;; when top level keys in the map is wildcarded
     [[(wild (comp keyword #(re-matches #"key\d+" %) name))] String]
     {:key0 "val0" :key1 "val1" :key2 "val2"}
     #{}

     ;; when top level keys in the map is wildcarded, but there are top
     ;; level keys which dont match the wildcard
     [[(wild (comp keyword #(re-matches #"key\d+" %) name))] String]
     {:key0 "val0" :key1 "val1" :key2 "val2" :top-level "another"}
       #{"Path [:top-level] was not specified in the schema."}

     ;; present concrete paths at the same level as the wildcard path,
     ;; wont be considered as an extraneous path
     [[:top-level] String
      [(wild (comp keyword #(re-matches #"key\d+" %) name))] String]
     {:key0 "val0" :key1 "val1" :key2 "val2" :top-level "another"}
     #{}

     ;; paths that are longer than the map accepts are handled without throwing exceptions
     [[:a (wild string?)] String]
     {:a 1}
       #{"Path [:a] was not specified in the schema."}

     ;; can't have empty maps at wilcard paths, they don't count
     [[:a :b] (strict-validation-schema [[(wild String)] Number])]
     {:a {}}
     #{"Map did not contain expected path [:a :b]."}

     ;; ... <continued>
     [[:a :b] (strict-validation-schema [[:banana-count] Number
                                         [(wild String)] Number])]
     {:a {}}
       #{"Map did not contain expected path [:a :b]."}

     ;;;; optional paths interactions with wild card paths

     ;; no missing path error even when it finds none that match the wildcard
     [(optional-path [:a (wild string?)]) String]
     {}
     #{}

     ;; no missing path error, even when the value isn't map as was expected
     [(optional-path [:a (wild string?)]) String]
     {:a 1}
       #{"Path [:a] was not specified in the schema."}

     ;; notices extraneous paths that have separately included subpaths
     ;; in same schema - regression test July 20, 2012
     [[:name]    String
      [:data]    map?     ;; this guy = 'separately included subpath'
      [:data :a] String]
     {:name "Roberto"
      :data {:a "cool"
             :b "dude"}}
     #{"Path [:data :b] was not specified in the schema."}
     ))

    (deftest test-valid?
      (testing "valid iff there'd be no error messages"
        (are [schema m result] (= (valid? schema m) result)

          [[:a] number?]
          {:a 1}
          true

          [[:b] number?]
          {}
          false)))

     (deftest test-optional-path
       (testing "optional paths are recognizable as such"
         (is (= true (optional-path? (optional-path [:a])))))

       (testing "optional paths have the same vector equality as their non-optional cousins"
         (is (= [:a] (optional-path [:a])))))

     (deftest test-sequence-of
       (testing "preds or schemas that have been marked as 'sequence-of' can be recognized"
         (is (= true (sequence-of? (sequence-of [[:a] string?]))))))

     (deftest test-schema-type-checkers
       (are [x pred] (pred x)
         nil (comp not schema?)
         nil (comp not loose-schema?)
         nil (comp not strict-schema?)
         (loose-validation-schema [[:a] string?]) schema?
         (loose-validation-schema [[:a] string?]) loose-schema?
         (loose-validation-schema [[:a] string?]) (comp not strict-schema?)
         loose-person-schema schema?
         loose-person-schema loose-schema?
         loose-person-schema (comp not strict-schema?)
         #'loose-person-schema schema?
         #'loose-person-schema loose-schema?
         #'loose-person-schema (comp not strict-schema?)
         (strict-validation-schema [[:a] string?]) schema?
         (strict-validation-schema [[:a] string?]) strict-schema?
         (strict-validation-schema [[:a] string?]) (comp not loose-schema?)
         family-schema schema?
         family-schema strict-schema?
         family-schema (comp not loose-schema?)
         #'family-schema schema?
         #'family-schema strict-schema?
         #'family-schema (comp not loose-schema?)))

     (deftest test-subtract-paths
       (doseq [[schema-identifier schema-maker] [[loose-schema? loose-validation-schema]
                                                 [strict-schema? strict-validation-schema]]]
         (is (= (subtract-paths (schema-maker [[:a] string? [:b] string? [:c] string?]) [:b] [:c])
               (schema-maker [[:a] string?])))
         (is (schema-identifier (subtract-paths (schema-maker [[:a] string? [:b] string? [:c] string?]) [:b] [:c])))))

     (deftest test-select-schema-keys
       (doseq [[schema-identifier schema-maker] [[loose-schema? loose-validation-schema]
                                                 [strict-schema? strict-validation-schema]]]
         (is (= (select-schema-keys (schema-maker [[:a :x] string? [:b :y] string? [:c :z] string?]) :b :c)
               (schema-maker [[:b :y] string? [:c :z] string?])))
         (is (schema-identifier (select-schema-keys (schema-maker [[:a :x] string? [:b :y] string? [:c :z] string?]) :b :c)))))

     (deftest test-schema-construction-preconditions
       (are [schema-maker args] (thrown? AssertionError (schema-maker args))
         loose-validation-schema [[:a] string? [:b string?]]
         strict-validation-schema [[:a] string? [:b string?]]
         loose-validation-schema ['(:a) string?]
         strict-validation-schema ['(:a) string?]))

     (deftest test-num-schema-paths
       (are [schema n] (= (num-schema-paths schema) n)
         family-schema 2
         []  0
         nil 0))


     ;; TODO ALex July 30, 2012 -- move into internal ns all about wildcard paths
     (deftest test-wildcard-path->concrete-paths
       (are [m wildcard-path concrete-paths] (= concrete-paths
                                               (wildcard-path->concrete-paths m
                                                 wildcard-path))
         ;; base cases
         {}
         []
         [[]]

         {:a 1}
         [:a]
         [[:a]]

         ;; expands concrete path into itself
         {:a {:any-keyword {:c {:any-keyword 'SOMETHING}}}}
         [:a :any-keyword :c :any-keyword]
         [[:a :any-keyword :c :any-keyword]]

         ;; shortest wildcard validator works -- important test don't remove
         {:a 1}
         [(wild keyword?)]
         [[:a]]

         ;; expands wildcard path into all possible paths based on the supplied map 'm'
         {:a {:b {:c "foo"}
              :x {:c "bar"}}}
         [:a (wild keyword?) :c]
         [[:a :b :c]
          [:a :x :c]]

         ;; if a map doesn't have enough nesting to satisfy a wildcard path, then
         ;; there are no concrete paths generated
         {:a 1}
         [:a (wild :b)]
         []

         ))

     ;; same here
     (deftest test-covered-by-wildcard-path?
       (are [path wildcard-path covered?] (= covered? (covered-by-wildcard-path? path wildcard-path))

         ;; base case
         []
         []
         true

         ;; ..
         [:a]
         [(wild keyword?)]
         true

         ;; ..
         [:a :b]
         [(wild keyword?)]
         false

         ;; ..
         [:a :b]
         [:a :b]
         true

         [:a]
         [:a :b]
         false

         [:a :b]
         [:a]
         false

         [:a "S"]
         [(wild keyword?) (wild String)]
         true))