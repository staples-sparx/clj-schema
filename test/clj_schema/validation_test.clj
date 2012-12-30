(ns clj-schema.validation-test
  (:use clojure.test
        clj-schema.schema
        clj-schema.validation
        clj-schema.test-schemas)
  (:import clojure.lang.Keyword))


(defn first-name-bob? [x]
  (-> x :name :first (= "Bob")))

(deftest test-validation-errors
  (are [schema m errors] (= errors (validation-errors (if (schema? schema) schema (map-schema :strict schema)) m))

;;;; Degenerate cases

       nil             {}          #{}
       []              {}          #{}
       nil             nil         #{}
       []              nil         #{}

       family-schema [[:a] 2 [:b] 4] #{"At path [], constraint failed. Expected '((fn [m] (or (nil? m) (map? m))) [[:a] 2 [:b] 4])' to be true, but was false."}

       ;;

;;;; One validator per path

       [[:a] number?]
       {:a 1}
       #{}

       [[:b] Integer]
       {}
       #{"Map did not contain expected path [:b]."}

       [[:bb] Integer]
       {:bb "bb"}
       #{"Value \"bb\" at path [:bb] expected class java.lang.Integer, but was java.lang.String"}

       [[:b :c] Integer]
       {:b "a"}
       #{"Path [:b] was not specified in the schema."
         "Map did not contain expected path [:b :c]."}

       [[:b :c] Integer]
       {:b {:c :a}}
       #{"Value :a at path [:b :c] expected class java.lang.Integer, but was clojure.lang.Keyword"}

       ;;

;;;; Multiple predicates per path - lists all failures, not just first

       [[:a] [number? pos?]]
       {:a 1}
       #{}

       [[:b] [String Keyword]]
       {}
       #{"Map did not contain expected path [:b]."}

       [[:b] [String Keyword]]
       {:b 1.1}
       #{"Value 1.1 at path [:b] expected class java.lang.String, but was java.lang.Double"
         "Value 1.1 at path [:b] expected class clojure.lang.Keyword, but was java.lang.Double"}

       [[:b :c] [number? pos?]]
       {:b "a"}
       #{"Path [:b] was not specified in the schema."
         "Map did not contain expected path [:b :c]."}

       [[:b :c] [String Keyword]]
       {:b {:c 1.1}}
       #{"Value 1.1 at path [:b :c] expected class java.lang.String, but was java.lang.Double"
         "Value 1.1 at path [:b :c] expected class clojure.lang.Keyword, but was java.lang.Double"}

       ;;

;;;; Handles combo of missing paths and erroneous values
       [[:b :c] [number? pos?]
        [:b :d] identity
        [:x :y] [neg? number?]
        [:z :z :top] keyword?]
       {:b {:c "a" :d :foo}
        :x {:y 99}}
       #{"Value \"a\", at path [:b :c], did not match predicate 'number?'."
         "Value \"a\", at path [:b :c], did not match predicate 'pos?'."
         "Map did not contain expected path [:z :z :top]."
         "Value 99, at path [:x :y], did not match predicate 'neg?'." }

;;;; when using 'sequence-of' validator is applied against each element in the sequence at that key
       [[:a] (sequence-of person-schema)]
       {:a [{:name {:first "Roberto"} :height 11} {:name {:first "Roberto"} :height "11"}]}
       #{"Value \"11\" at path [:a :height] expected class java.lang.Number, but was java.lang.String"}

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
       #{"Value \"70\" at path [:a :height] expected class java.lang.Number, but was java.lang.String"}

;;;; you can mix types of validators: preds with schemas
       [[:a] (sequence-of [first-name-bob? person-schema])]
       {:a [{:name {:first "Roberto"} :height 44} {:name {:first "Chris"} :height "4a"}]}
       #{"Value \"4a\" at path [:a :height] expected class java.lang.Number, but was java.lang.String"
         "Value {:name {:first \"Roberto\"}, :height 44}, at path [:a], did not match predicate 'clj-schema.validation-test/first-name-bob?'."
         "Value {:name {:first \"Chris\"}, :height \"4a\"}, at path [:a], did not match predicate 'clj-schema.validation-test/first-name-bob?'."}

;;;; ...  multiple strict schemas together makes little sense - one schema will think extra keys were not specified by it, though they were by the other schema
       [[:a] (sequence-of [name-schema person-schema])]
       {:a [{:name {:first :Roberto} :height 69} {:name {:first "Roberto"} :height "69"}]}
       #{"Value \"69\" at path [:a :height] expected class java.lang.Number, but was java.lang.String"
         "Value :Roberto at path [:a :name :first] expected class java.lang.String, but was clojure.lang.Keyword"
         "Path [:a :height] was not specified in the schema."}

;;;; validator on right of schema, can be made an or, and can work with both preds and schemas mixed
       [[:a] [:or nil? person-schema]]
       {:a nil}
       #{}

;;;; you can have just one thing in the ':or' - but please don't it is weird
       [[:a] (sequence-of [:or person-schema])]
       {:a [{:name {:first "Roberto"} :height "76"}]}
       #{"Value \"76\" at path [:a :height] expected class java.lang.Number, but was java.lang.String"}

;;;; when both :or options fail - see errors for both 'nil?' and 'person-schema'
       [[:a] [:or nil? person-schema]]
       {:a {:name {:first "Roberto"} :height "66"}}
       #{"Value {:name {:first \"Roberto\"}, :height \"66\"}, at path [:a], did not match predicate 'nil?'."
         "Value \"66\" at path [:a :height] expected class java.lang.Number, but was java.lang.String"}

;;;; or collects all failures in the sequence being checked
       [[:a] (sequence-of [:or nil? person-schema])]
       {:a [{:name {:first "Roberto"} :height "88"} {:name {:first "Roberto"} :height 88}]}
       #{"Value \"88\" at path [:a :height] expected class java.lang.Number, but was java.lang.String"
         "Value {:name {:first \"Roberto\"}, :height \"88\"}, at path [:a], did not match predicate 'nil?'."}

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
       #{"Value :53 at path [:a :family :dad :height] expected class java.lang.Number, but was clojure.lang.Keyword"
         "Value :Theresa at path [:a :family :mom :name :first] expected class java.lang.String, but was clojure.lang.Keyword"
         }

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

;;;; marked as 'sequence-of' but only one value - causes an error
       [[:a] (sequence-of person-schema)]
       {:a {:name {:first "Roberto"} :height "76"}}
       #{"At path [:a], constraint failed. Expected '((fn [m] (or (nil? m) (sequential? m))) {:name {:first \"Roberto\"}, :height \"76\"})' to be true, but was false."}

;;;; using 'sequence-of' with an 'Number' class - means there is a seq of numbers
       [[:a] (sequence-of Number)]
       {:a 1}
       #{"At path [:a], constraint failed. Expected '((fn [m] (or (nil? m) (sequential? m))) 1)' to be true, but was false."}

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

;;;; marked as 'set-of' but only one value - causes an error
       [[:a] (set-of person-schema)]
       {:a {:name {:first "Roberto"} :height "76"}}
       #{"At path [:a], constraint failed. Expected '((fn [m] (or (nil? m) (set? m))) {:name {:first \"Roberto\"}, :height \"76\"})' to be true, but was false."}

;;;; using 'set-of' with an 'Number' predicate - means there is a set of numbers
       [[:a] (set-of Number)]
       {:a 1}
       #{"At path [:a], constraint failed. Expected '((fn [m] (or (nil? m) (set? m))) 1)' to be true, but was false."}

;;;; nil is an acceptable value for a 'set-of' validator
       [[:a] (set-of integer?)]
       {:a nil}
       #{}

;;;; set-of can be used from within other nested validators -- #{:a} is single item
       [[:a] [:or Number (set-of Number)]]
       {:a 4}
       #{}

;;;; ... <continued from above> -- [:a] is sequential
       [[:a] [:or Number (set-of Number)]]
       {:a #{4 5 6 7}}
       #{}



     

;;;; nested loose schemas don't count toward strict schema's keys
       [[:a] loose-height-schema]
       {:a {:height 72 :extra-key-doesnt-cuase-error "foo"}
        :b "oops"}
       #{"Path [:b] was not specified in the schema."}

;;;; can use Classes as a validator
       [[:a] String]
       {:a "Roberto"}
       #{}

       [[:a] String]
       {:a :Roberto}
       #{"Value :Roberto at path [:a] expected class java.lang.String, but was clojure.lang.Keyword"}

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
       #{"Value :b at path [:a :x \"b\"] expected class java.lang.String, but was clojure.lang.Keyword"}

       ;; if a path exists that doesn't match the wildcard, it is considered an extraneous path
       [[:a] (map-schema :strict [[(wild Keyword)] String])]
       {:a {"b" "foo" "c" "bar"}}
       #{"Path [:a \"c\"] was not specified in the schema."
         "Path [:a \"b\"] was not specified in the schema."}

       ;; can use Class objects as wildcard part of wildcard path
       [[:a (wild String)] String]
       {:a {"b" :b "c" "letter c"}}
       #{"Value :b at path [:a \"b\"] expected class java.lang.String, but was clojure.lang.Keyword"}

       ;; can use 'and statements' in validators
       [[:a (wild [String #{"baz" "qux"}])] String]
       {:a {"baz" :b "c" "letter c"}}
       #{"Value :b at path [:a \"baz\"] expected class java.lang.String, but was clojure.lang.Keyword"
         "Path [:a \"c\"] was not specified in the schema."}

       ;; if no keys of the leaf-map match the wildcard-validator, that is OK
       [[:a (wild string?)] String]
       {:a {999 :boom}}
       #{"Path [:a 999] was not specified in the schema."}

       ;; Wildcard paths match empty maps
       [[:a (wild string?)] String]
       {:a {}}
       #{}

       ;; don't get missing path errors for nested schemas within wildcard paths - regression test Jun 15, 2012
       [[:carted (wild String)] product-schema]
       {:carted {"Sneetch" {:quantity 5 :price 100}}}
       #{}

       ;; doesn't confuse paths with string keys as wildcard paths - regression test Jun 15, 2012
       [["Sneetch" :unit-price-cents] String]
       {"Sneetch" {:unit-price-cents "a"}}
       #{}

       ;; won't confuse them in nested paths either
       [["Sneetch" :unit-price-cents] (map-schema :strict [[:a] string?])]
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
       [[:a :b] (map-schema :strict [[(wild String)] Number])]
       {:a {}}
       #{"Map did not contain expected path [:a :b]."}

       ;; ... <continued>
       [[:a :b] (map-schema :strict [[:banana-count] Number
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
        [:data]    map? ;; this guy = 'separately included subpath'
        [:data :a] String]
       {:name "Roberto"
        :data {:a "cool"
               :b "dude"}}
       #{"Path [:data :b] was not specified in the schema."}


       ;; [Issue #1] - Can AND a sequential with a single item validator
       [[:a] [empty? (sequence-of String)]]
       {:a []}
       #{}

       ;; [Issue #1] - continued...
       [[:a] [empty? (sequence-of String)]]
       {:a ["Roberto"]}
       #{"Value [\"Roberto\"], at path [:a], did not match predicate 'empty?'."}

       ))

(deftest test-schemas-can-check-constraints-against-entire-map
  (let [errors (validation-errors schema-with-constraints {:a "string"
                                                           :b 99
                                                           :extra 47})]
    (is (or (= #{"At path [], constraint failed. Expected '((fn [m] (even? (count (keys m)))) {:extra 47, :a \"string\", :b 99})' to be true, but was false." "At path [], constraint failed. Expected '((comp even? count distinct vals) {:extra 47, :a \"string\", :b 99})' to be true, but was false."}
               errors)
            (= #{"At path [], constraint failed. Expected '((fn [m] (even? (count (keys m)))) {:a \"string\", :b 99, :extra 47})' to be true, but was false." "At path [], constraint failed. Expected '((comp even? count distinct vals) {:a \"string\", :b 99, :extra 47})' to be true, but was false."}
               errors))))

  (is (= #{} (validation-errors schema-with-constraints {:a "string"
                                                         :b 99}))))

       (deftest test-loose-schema-validations
         (are [schema m errors] (= errors (validation-errors (map-schema :loose schema) m))

           ;; extra paths on wild card paths are ok if the schema is loose
           [[:a (wild string?)] String]
           {:a {999 :boom}}
             #{}
           ))

       (deftest test-valid?
      (testing "valid iff there'd be no error messages"
        (are [schema m result] (= (valid? (map-schema :strict schema) m) result)

          [[:a] number?]
          {:a 1}
          true

          [[:b] number?]
          {}
          false)))

     ;; TODO ALex July 30, 2012 -- move into internal ns all about wildcard paths
     (deftest test-wildcard-path->concrete-paths
       (are [m wildcard-path concrete-paths] (= (set concrete-paths)
                                               (set (#'clj-schema.validation/wildcard-path->concrete-paths m
                                                      wildcard-path)))
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
  (are [path wildcard-path covered?] (= covered? (#'clj-schema.validation/covered-by-wildcard-path? path wildcard-path))

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

(def-map-schema foo-schema
  [[:a] String])

(deftest test-validate-and-handle
  (is (= "SUCCESS: {:a \"one\"}"
         (validate-and-handle {:a "one"}
                              foo-schema
                              (fn [m] (str "SUCCESS: " m))
                              (fn [m errors] (str "FAIL: " m errors)))))
  (is (= "FAIL: {:b 2}(\"Map did not contain expected path [:a].\" \"Path [:b] was not specified in the schema.\")"
         (validate-and-handle {:b 2}
                              foo-schema
                              (fn [m] (str "SUCCESS: " m))
                              (fn [m errors] (str "FAIL: " m errors))))))

(deftest test-seq-validation-errors
  (is (= #{}
         (validation-errors (seq-schema String) ["a" "b" "c"])))
  (is (= #{"Value :c at path [] expected class java.lang.String, but was clojure.lang.Keyword"
           "Value :b at path [] expected class java.lang.String, but was clojure.lang.Keyword"
           "Value :a at path [] expected class java.lang.String, but was clojure.lang.Keyword"}
         (validation-errors (seq-schema String) [:a :b :c]))))

(deftest test-simple-schemas
  (is (= #{}
         (validation-errors String "neat")))
  (is (= #{"Value 44 at path [] expected class java.lang.String, but was java.lang.Integer"}
         (validation-errors String 44)))

  (is (= #{}
         (validation-errors [:or String Number] "neat")
         (validation-errors [:or String Number] 55)))
  (is (= #{"Value :keyword at path [] expected class java.lang.String, but was clojure.lang.Keyword"
           "Value :keyword at path [] expected class java.lang.Number, but was clojure.lang.Keyword"}
         (validation-errors [:or String Number] :keyword)))

  (is (= #{}
         (validation-errors [Number Long] (long 55))))
  (is (= #{"Value :keyword at path [] expected class java.lang.Long, but was clojure.lang.Keyword"
           "Value :keyword at path [] expected class java.lang.Number, but was clojure.lang.Keyword"}
         (validation-errors [Number Long] :keyword)))

  (is (= #{}
         (validation-errors string? "string")))
  (is (= #{"Value 99, at path [], did not match predicate 'string?'."}
         (validation-errors string? 99))))