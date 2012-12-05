(ns clj-schema.schema-test
  (:use clojure.test
        clj-schema.schema
        clj-schema.test-schemas)
  (:import clojure.lang.Keyword))


(deftest all-public-vars-have-docstrings
  (is (= [] (map str (remove (comp :doc meta) (vals (ns-publics 'clj-schema.schema))))))
  (is (= [] (map str (remove (comp :doc meta) (vals (ns-publics 'clj-schema.validation))))))
  (is (= [] (map str (remove (comp :doc meta) (vals (ns-publics 'clj-schema.fixtures)))))))


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
       (loose-schema [[:a] string?]) (comp not strict-schema?)
       loose-person-schema schema?
       loose-person-schema loose-schema?
       loose-person-schema (comp not strict-schema?)
       #'loose-person-schema schema?
       #'loose-person-schema loose-schema?
       #'loose-person-schema (comp not strict-schema?)
       (strict-schema [[:a] string?]) schema?
       (strict-schema [[:a] string?]) strict-schema?
       (strict-schema [[:a] string?]) (comp not loose-schema?)
       family-schema schema?
       family-schema strict-schema?
       family-schema (comp not loose-schema?)
       #'family-schema schema?
       #'family-schema strict-schema?
       #'family-schema (comp not loose-schema?)))

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
        strict-schema [[:a] string? [:b string?]]
        loose-schema ['(:a) string?]
        strict-schema ['(:a) string?]))

 (deftest test-num-schema-paths
   (are [schema n] (= (num-schema-paths schema) n)
        family-schema 2
        []  0
        nil 0))

(deftest test-optional-path-set
  (is (= #{} (optional-path-set [])))
  (is (= #{} (optional-path-set [[:a] String])))
  (is (= #{[:a] [:b]} (optional-path-set [(optional-path [:b]) String
                                          [:c] pos?
                                          (optional-path [:a]) Integer]))))

(deftest test-scaffold-schema
  (is (= '(defschema foo
            [[:a] Anything
             [:b :c] Anything])
         (scaffold-schema "foo" {:a 1234
                                 :b {:c "asdf"}}))))
