(ns clj-schema.module-specs-test
  (:require [clojure.test :refer :all]
            [clj-schema.module-specs :refer :all]
            [clj-schema.schema :as schema]))


(defn f [a & bs]
  (keyword (apply str a bs)))

(def my-specs
  [{:module-entry-point-var #'f
    :input-schema (schema/sequence-of [:or String clojure.lang.Keyword])
    :output-schema String}])

(deftest test-hooking-module-specs
  (f "a" :b :c)

  (add-module-specs-hooks! my-specs)

  (is (thrown-with-msg? Exception #"Errors found in inputs"
        (f "a" 'b 'c)))

  (is (thrown-with-msg? Exception #"Errors found in outputs"
        (f "a" "b" :c)))

  (remove-module-spec-hooks! my-specs)
  (f "a" 'b 'c))

(def input-state (atom nil))
(def output-state (atom nil))

(def your-specs
  [{:module-entry-point-var #'f
    :input-schema (schema/sequence-of [:or String clojure.lang.Keyword])
    :input-schema-on-failure (fn [f input errors]
                               (reset! input-state [f input errors]))
    :output-schema String
    :output-schema-on-failure (fn [f input errors]
                                (reset! output-state [f input errors]))}])

(deftest test-specifying-on-failure-handlers
  (add-module-specs-hooks! your-specs)

  (f "a" 'b 'c)
  (is (= [#'clj-schema.module-specs-test/f
          ["a" 'b 'c]
          #{"Expected value c, at path [2], to be an instance of class java.lang.String, but was clojure.lang.Symbol"
            "Expected value b, at path [1], to be an instance of class java.lang.String, but was clojure.lang.Symbol"
            "Expected value c, at path [2], to be an instance of class clojure.lang.Keyword, but was clojure.lang.Symbol"
            "Expected value b, at path [1], to be an instance of class clojure.lang.Keyword, but was clojure.lang.Symbol"}]
         @input-state))

  (f "a" "b" "c")
  (is (= [#'clj-schema.module-specs-test/f
          :abc
          #{"Expected value :abc to be an instance of class java.lang.String, but was clojure.lang.Keyword"}]
         @output-state))

  (remove-module-spec-hooks! your-specs)
  (f "a" 'b 'c))