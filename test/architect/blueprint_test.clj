(ns architect.blueprint-test
  (:use clojure.test
        architect.blueprint
        architect.test-blueprints)
  (:import clojure.lang.Keyword))


(deftest all-public-vars-have-docstrings
  (is (= [] (map str (remove (comp :doc meta) (vals (ns-publics 'architect.blueprint))))))
  (is (= [] (map str (remove #(= '->StringErrorReporter (:name (meta %))) (remove (comp :doc meta) (vals (ns-publics 'architect.validation)))))))
  (is (= [] (map str (remove (comp :doc meta) (vals (ns-publics 'architect.example)))))))


;;; Creation

(deftest test-loose-blueprint-and-as-strict-schmema
  (is (= {:type :map
          :blueprint-spec [[:a] {:type :predicate, :blueprint-spec even?, :constraints [], :source nil}
                        [:b] {:type :class, :blueprint-spec java.lang.String, :constraints []}
                        [:a] {:type :class, :blueprint-spec java.lang.Number, :constraints []}]
  :constraints @#'architect.blueprint/map-constraints
  :strict false}
          (map-blueprint :loose [[:a] even?]
                             [[:b] String
                              [:a] Number]))))

(deftest test-strict-blueprint-and-as-loose-blueprint
  (is (= {:type :map
          :blueprint-spec [[:a] {:type :predicate, :blueprint-spec even?, :constraints [], :source nil}
                        [:b] {:type :class, :blueprint-spec java.lang.String, :constraints []}
                        [:a] {:type :class, :blueprint-spec java.lang.Number, :constraints []}]
          :constraints @#'architect.blueprint/map-constraints
          :strict true}
         (map-blueprint :strict
                     [[:a] even?]
                     [[:b] String
                      [:a] Number]))))

(deftest test-def-loose-blueprint
  (is (= {:type :map
          :blueprint-spec [[:name :first] {:type :class, :blueprint-spec java.lang.String, :constraints []}
                        [:height] {:type :class, :blueprint-spec java.lang.Number, :constraints []}]
          :constraints @#'architect.blueprint/map-constraints
          :strict false}
         loose-person-blueprint)))

(deftest test-def-map-blueprint
  (is (= {:type :map
          :blueprint-spec [[:name :first] {:type :class, :blueprint-spec java.lang.String, :constraints []}
                        [:height] {:type :class, :blueprint-spec java.lang.Number, :constraints []}]
          :constraints @#'architect.blueprint/map-constraints
          :strict true}
         person-blueprint)))

(deftest test-constraints
  (is (= {:architect.blueprint/constraint-bundle [{:type :predicate
                                                 :blueprint-spec first
                                                 :constraints []
                                                 :source 'first}
                                                {:type :predicate
                                                 :blueprint-spec second
                                                 :constraints []
                                                 :source 'second}]}
         (constraints first
                      second)))

  (is (= true ((:blueprint-spec (first (:architect.blueprint/constraint-bundle (constraints odd?)))) 99)))
  (is (= false ((:blueprint-spec (first (:architect.blueprint/constraint-bundle (constraints odd?)))) 100))))

(deftest test-blueprints-with-constraints
  (is (= 4 (count (keys blueprint-with-constraints))))
  (is (= [[:a] {:type :class, :blueprint-spec java.lang.String, :constraints []}
          [:b] {:type :class, :blueprint-spec java.lang.Number, :constraints []}]
         (:blueprint-spec blueprint-with-constraints)))
  (is (= false (:strict blueprint-with-constraints)))
  (is (= 3 (count (:constraints blueprint-with-constraints))))
  (is (every? blueprint? (:constraints blueprint-with-constraints))))


;;; Questions

(deftest test-blueprint-path-set
  (is (= #{[:mom] [:dad]} (blueprint-path-set family-blueprint))))

(deftest test-blueprint-rows
  (is (= [[[:name :first] {:type :class, :blueprint-spec java.lang.String, :constraints []}]
          [[:height] {:type :class, :blueprint-spec java.lang.Number, :constraints []}]]
         )
  (is (= [[[:mom] {:type :map
                   :blueprint-spec [[:name :first] {:type :class, :blueprint-spec java.lang.String, :constraints []}
                                 [:height] {:type :class, :blueprint-spec java.lang.Number, :constraints []}]
                   :constraints @#'architect.blueprint/map-constraints
                   :strict true}]
          [[:dad] {:type :map
                   :blueprint-spec [[:name :first] {:type :class, :blueprint-spec java.lang.String, :constraints []}
                                 [:height] {:type :class, :blueprint-spec java.lang.Number, :constraints []}]
                   :constraints @#'architect.blueprint/map-constraints
                   :strict true}]]
         (blueprint-rows family-blueprint)))))

(deftest test-optional-path
  (testing "optional paths are recognizable as such"
    (is (= true (optional-path? (optional-path [:a])))))

  (testing "optional paths have the same vector equality as their non-optional cousins"
    (is (= [:a] (optional-path [:a])))))

(deftest test-blueprint-type-checkers
  (are [x pred] (pred x)
       nil (comp not blueprint?)
       nil (comp not loose-blueprint?)
       nil (comp not strict-blueprint?)
       
       (map-blueprint :loose [[:a] string?]) blueprint?
       (map-blueprint :loose [[:a] string?]) loose-blueprint?
       (map-blueprint :loose [[:a] string?]) (complement strict-blueprint?)
       
       loose-person-blueprint blueprint?
       loose-person-blueprint loose-blueprint?
       loose-person-blueprint (complement strict-blueprint?)
       
       #'loose-person-blueprint blueprint?
       #'loose-person-blueprint loose-blueprint?
       #'loose-person-blueprint (complement strict-blueprint?)
       
       (map-blueprint :strict [[:a] string?]) blueprint?
       (map-blueprint :strict [[:a] string?]) strict-blueprint?
       (map-blueprint :strict [[:a] string?]) (complement loose-blueprint?)
       
       family-blueprint blueprint?
       family-blueprint strict-blueprint?
       family-blueprint (complement loose-blueprint?)
       
       #'family-blueprint blueprint?
       #'family-blueprint strict-blueprint?
       #'family-blueprint (complement loose-blueprint?)

       #'my-seq-blueprint blueprint?
       #'my-set-blueprint blueprint?
       #'my-simple-blueprint blueprint?))

(deftest test-subtract-paths
  (doseq [[blueprint-identifier looseness] [[loose-blueprint? :loose]
                                         [strict-blueprint? :strict]]]
    (is (= (subtract-paths (map-blueprint looseness [[:a] string? [:b] string? [:c] string?]) [:b] [:c])
           (map-blueprint looseness [[:a] string?])))
    (is (blueprint-identifier (subtract-paths (map-blueprint looseness [[:a] string? [:b] string? [:c] string?]) [:b] [:c])))))

(deftest test-select-blueprint-keys
  (doseq [[blueprint-identifier looseness] [[loose-blueprint? :loose]
                                         [strict-blueprint? :strict]]]
    (is (= (select-blueprint-keys (map-blueprint looseness [[:a :x] string? [:b :y] string? [:c :z] string?]) :b :c)
           (map-blueprint looseness [[:b :y] string? [:c :z] string?])))
    (is (blueprint-identifier (select-blueprint-keys (map-blueprint looseness [[:a :x] string? [:b :y] string? [:c :z] string?]) :b :c)))))

(deftest test-blueprint-construction-preconditions
  (is (thrown? AssertionError (map-blueprint :loose [[:a] string? [:b string?]])))
  (is (thrown? AssertionError (map-blueprint :strict [[:a] string? [:b string?]]))))

(deftest test-optional-path-set
  (is (= #{} (optional-path-set (map-blueprint :strict []))))
  (is (= #{} (optional-path-set (map-blueprint :strict [[:a] String]))))
  (is (= #{[:a] [:b]} (optional-path-set (map-blueprint :strict [(optional-path [:b]) String
                                                              [:c] pos?
                                                              (optional-path [:a]) Integer])))))

(deftest test-scaffold-blueprint
  (is (= '(def-map-blueprint foo
            [[:a] Anything
             [:b :c] Anything])
         (scaffold-blueprint "foo" {:a 1234
                                 :b {:c "asdf"}})))

  (is (= '(def-seq-blueprint foo
            Anything)
         (scaffold-blueprint "foo" [1 2 3])))

  (is (= '(def-set-blueprint foo
            Anything)
         (scaffold-blueprint "foo" #{1 2 3}))))
