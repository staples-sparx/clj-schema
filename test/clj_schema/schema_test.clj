(ns clj-schema.schema-test
  (:use clojure.test
        clj-schema.schema
        clj-schema.test-schemas)
  (:import clojure.lang.Keyword))


(deftest all-public-vars-have-docstrings
  (is (= [] (map str (remove (comp :doc meta) (vals (ns-publics 'clj-schema.schema))))))
  (is (= [] (map str (remove (comp :doc meta) (vals (ns-publics 'clj-schema.validation))))))
  (is (= [] (map str (remove (comp :doc meta) (vals (ns-publics 'clj-schema.fixtures)))))))


;;; Creation

(deftest test-loose-schema-and-as-strict-schmema
  (let [loose (loose-schema  [[:a] even?]
                             [[:b] String
                              [:a] Number])]
    (is (= {:schema [[:a] even?
                     [:b] String
                     [:a] Number]
            :constraints []
            :strict false}) loose)
    (is (= {:schema [[:a] even?
                     [:b] String
                     [:a] Number]
            :constraints []
            :strict true}
           (as-strict-schema loose)))))

(deftest test-strict-schema-and-as-loose-schema
  (let [strict (strict-schema  [[:a] even?]
                               [[:b] String
                                [:a] Number])]
    (is (= {:schema [[:a] even?
                     [:b] String
                     [:a] Number]
            :constraints []
            :strict true}) strict)
    (is (= {:schema [[:a] even?
                     [:b] String
                     [:a] Number]
            :constraints []
            :strict false}
           (as-loose-schema strict)))))

(deftest test-def-loose-schema
  (is (= {:schema [[:name :first] java.lang.String
                   [:height] java.lang.Number]
          :constraints []
          :strict false}
         loose-person-schema)))

(deftest test-defschema
  (is (= {:schema [[:name :first] java.lang.String [:height] java.lang.Number]
          :constraints []
          :strict true}
         person-schema)))

(deftest test-constraints
  (is (= [{:predicate first
           :source 'first}
          {:predicate second
           :source 'second}]
         (constraints first
                      second))))

(deftest test-schemas-with-constraints
  (is (= 3 (count (keys schema-with-constraints))))
  (is (= [[:a] java.lang.String [:b] java.lang.Number] (:schema schema-with-constraints)))
  (is (= false (:strict schema-with-constraints)))
  (is (= 2 (count (:constraints schema-with-constraints))))
  (is (every? constraint? (:constraints schema-with-constraints))))


;;; Questions

(deftest test-schema-path-set
  (is (= #{[:mom] [:dad]} (schema-path-set family-schema))))

(deftest test-schema-rows
  (is (= [[[:name :first] java.lang.String]
          [[:height] java.lang.Number]]
         (schema-rows loose-person-schema)))
  (is (= [[[:mom] {:schema [[:name :first] java.lang.String
                            [:height] java.lang.Number]
                   :constraints []
                   :strict true}]
          [[:dad] {:schema [[:name :first] java.lang.String
                            [:height] java.lang.Number]
                   :constraints []
                   :strict true}]]
         (schema-rows family-schema))))

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
       
       (loose-schema [[:a] string?]) schema?
       (loose-schema [[:a] string?]) loose-schema?
       (loose-schema [[:a] string?]) (complement strict-schema?)
       
       loose-person-schema schema?
       loose-person-schema loose-schema?
       loose-person-schema (complement strict-schema?)
       
       #'loose-person-schema schema?
       #'loose-person-schema loose-schema?
       #'loose-person-schema (complement strict-schema?)
       
       (strict-schema [[:a] string?]) schema?
       (strict-schema [[:a] string?]) strict-schema?
       (strict-schema [[:a] string?]) (complement loose-schema?)
       
       family-schema schema?
       family-schema strict-schema?
       family-schema (complement loose-schema?)
       
       #'family-schema schema?
       #'family-schema strict-schema?
       #'family-schema (complement loose-schema?)

       (as-strict-schema (loose-schema [[:a] string?])) schema?
       (as-strict-schema (loose-schema [[:a] string?])) strict-schema?
       (as-strict-schema (loose-schema [[:a] string?])) (complement loose-schema?)

       (as-loose-schema (strict-schema [[:a] string?])) schema?
       (as-loose-schema (strict-schema [[:a] string?])) loose-schema?
       (as-loose-schema (strict-schema [[:a] string?])) (complement strict-schema?)))

(deftest test-subtract-paths
  (doseq [[schema-identifier schema-maker] [[loose-schema? loose-schema]
                                            [strict-schema? strict-schema]]]
    (is (= (subtract-paths (schema-maker [[:a] string? [:b] string? [:c] string?]) [:b] [:c])
           (schema-maker [[:a] string?])))
    (is (schema-identifier (subtract-paths (schema-maker [[:a] string? [:b] string? [:c] string?]) [:b] [:c])))))

(deftest test-select-schema-keys
  (doseq [[schema-identifier schema-maker] [[loose-schema? loose-schema]
                                            [strict-schema? strict-schema]]]
    (is (= (select-schema-keys (schema-maker [[:a :x] string? [:b :y] string? [:c :z] string?]) :b :c)
           (schema-maker [[:b :y] string? [:c :z] string?])))
    (is (schema-identifier (select-schema-keys (schema-maker [[:a :x] string? [:b :y] string? [:c :z] string?]) :b :c)))))

(deftest test-schema-construction-preconditions
   (are [schema-maker args] (thrown? AssertionError (schema-maker args))
        loose-schema [[:a] string? [:b string?]]
        strict-schema [[:a] string? [:b string?]]))

(deftest test-optional-path-set
  (is (= #{} (optional-path-set (strict-schema []))))
  (is (= #{} (optional-path-set (strict-schema [[:a] String]))))
  (is (= #{[:a] [:b]} (optional-path-set (strict-schema [(optional-path [:b]) String
                                                         [:c] pos?
                                                         (optional-path [:a]) Integer])))))

(deftest test-scaffold-schema
  (is (= '(defschema foo
            [[:a] Anything
             [:b :c] Anything])
         (scaffold-schema "foo" {:a 1234
                                 :b {:c "asdf"}}))))
