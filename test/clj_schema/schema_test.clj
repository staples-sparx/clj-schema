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
  (let [loose (map-schema :loose
                          [[:a] even?]
                          [[:b] String
                           [:a] Number])]
    (is (= {:type :map
            :schema-spec [[:a] even?
                          [:b] String
                          [:a] Number]
            :constraints @#'clj-schema.schema/map-constraints
            :strict false}) loose)))

(deftest test-strict-schema-and-as-loose-schema
  (is (= {:type :map
          :schema-spec [[:a] even?
                        [:b] String
                        [:a] Number]
          :constraints @#'clj-schema.schema/map-constraints
          :strict true}
         (map-schema :strict
                     [[:a] even?]
                     [[:b] String
                      [:a] Number]))))

(deftest test-def-loose-schema
  (is (= {:type :map
          :schema-spec [[:name :first] java.lang.String
                        [:height] java.lang.Number]
          :constraints @#'clj-schema.schema/map-constraints
          :strict false}
         loose-person-schema)))

(deftest test-def-map-schema
  (is (= {:type :map
          :schema-spec [[:name :first] java.lang.String [:height] java.lang.Number]
          :constraints @#'clj-schema.schema/map-constraints
          :strict true}
         person-schema)))

(deftest test-constraints
  (is (= {:clj-schema.schema/constraint-bundle [{:type :predicate
                                                 :schema-spec first
                                                 :constraints []
                                                 :source 'first}
                                                {:type :predicate
                                                 :schema-spec second
                                                 :constraints []
                                                 :source 'second}]}
         (constraints first
                      second)))

  (is (= true ((:schema-spec (first (:clj-schema.schema/constraint-bundle (constraints odd?)))) 99)))
  (is (= false ((:schema-spec (first (:clj-schema.schema/constraint-bundle (constraints odd?)))) 100))))

(deftest test-schemas-with-constraints
  (is (= 4 (count (keys schema-with-constraints))))
  (is (= [[:a] java.lang.String
          [:b] java.lang.Number]
         (:schema-spec schema-with-constraints)))
  (is (= false (:strict schema-with-constraints)))
  (is (= 3 (count (:constraints schema-with-constraints))))
  (is (every? schema? (:constraints schema-with-constraints))))


;;; Questions

(deftest test-schema-path-set
  (is (= #{[:mom] [:dad]} (schema-path-set family-schema))))

(deftest test-schema-rows
  (is (= [[[:name :first] java.lang.String]
          [[:height] java.lang.Number]]
         (schema-rows loose-person-schema)))
  (is (= [[[:mom] {:type :map
                   :schema-spec [[:name :first] java.lang.String
                                 [:height] java.lang.Number]
                   :constraints @#'clj-schema.schema/map-constraints
                   :strict true}]
          [[:dad] {:type :map
                   :schema-spec [[:name :first] java.lang.String
                                 [:height] java.lang.Number]
                   :constraints @#'clj-schema.schema/map-constraints
                   :strict true}]]
         (schema-rows family-schema))))

(deftest test-optional-path
  (testing "optional paths are recognizable as such"
    (is (= true (optional-path? (optional-path [:a])))))

  (testing "optional paths have the same vector equality as their non-optional cousins"
    (is (= [:a] (optional-path [:a])))))

(deftest test-schema-type-checkers
  (are [x pred] (pred x)
       nil (comp not schema?)
       nil (comp not loose-schema?)
       nil (comp not strict-schema?)
       
       (map-schema :loose [[:a] string?]) schema?
       (map-schema :loose [[:a] string?]) loose-schema?
       (map-schema :loose [[:a] string?]) (complement strict-schema?)
       
       loose-person-schema schema?
       loose-person-schema loose-schema?
       loose-person-schema (complement strict-schema?)
       
       #'loose-person-schema schema?
       #'loose-person-schema loose-schema?
       #'loose-person-schema (complement strict-schema?)
       
       (map-schema :strict [[:a] string?]) schema?
       (map-schema :strict [[:a] string?]) strict-schema?
       (map-schema :strict [[:a] string?]) (complement loose-schema?)
       
       family-schema schema?
       family-schema strict-schema?
       family-schema (complement loose-schema?)
       
       #'family-schema schema?
       #'family-schema strict-schema?
       #'family-schema (complement loose-schema?)

       #'my-seq-schema schema?
       #'my-set-schema schema?
       #'my-simple-schema schema?))

(deftest test-subtract-paths
  (doseq [[schema-identifier looseness] [[loose-schema? :loose]
                                         [strict-schema? :strict]]]
    (is (= (subtract-paths (map-schema looseness [[:a] string? [:b] string? [:c] string?]) [:b] [:c])
           (map-schema looseness [[:a] string?])))
    (is (schema-identifier (subtract-paths (map-schema looseness [[:a] string? [:b] string? [:c] string?]) [:b] [:c])))))

(deftest test-select-schema-keys
  (doseq [[schema-identifier looseness] [[loose-schema? :loose]
                                         [strict-schema? :strict]]]
    (is (= (select-schema-keys (map-schema looseness [[:a :x] string? [:b :y] string? [:c :z] string?]) :b :c)
           (map-schema looseness [[:b :y] string? [:c :z] string?])))
    (is (schema-identifier (select-schema-keys (map-schema looseness [[:a :x] string? [:b :y] string? [:c :z] string?]) :b :c)))))

(deftest test-schema-construction-preconditions
  (is (thrown? AssertionError (map-schema :loose [[:a] string? [:b string?]])))
  (is (thrown? AssertionError (map-schema :strict [[:a] string? [:b string?]]))))

(deftest test-optional-path-set
  (is (= #{} (optional-path-set (map-schema :strict []))))
  (is (= #{} (optional-path-set (map-schema :strict [[:a] String]))))
  (is (= #{[:a] [:b]} (optional-path-set (map-schema :strict [(optional-path [:b]) String
                                                              [:c] pos?
                                                              (optional-path [:a]) Integer])))))

(deftest test-scaffold-schema
  (is (= '(def-map-schema foo
            [[:a] Anything
             [:b :c] Anything])
         (scaffold-schema "foo" {:a 1234
                                 :b {:c "asdf"}})))

  (is (= '(def-seq-schema foo
            Anything)
         (scaffold-schema "foo" [1 2 3])))

  (is (= '(def-set-schema foo
            Anything)
         (scaffold-schema "foo" #{1 2 3}))))
