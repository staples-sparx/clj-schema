(ns ^:alpha clj-schema.contracts-test
  (:use clojure.test
        clj-schema.contracts)
  (:require [clj-schema.schema :as schema]))


(defn f [a & bs]
  (keyword (apply str a bs)))

(def my-contracts
  [{:var #'f
    :input-schema (schema/sequence-of [:or String clojure.lang.Keyword])
    :output-schema String}])

(deftest test-adding-contracts
  (f "a" :b :c)

  (add-contracts! my-contracts)

  (is (thrown-with-msg? Exception #"Errors found in inputs"
        (f "a" 'b 'c)))

  (is (thrown-with-msg? Exception #"Errors found in outputs"
        (f "a" "b" :c)))

  (remove-contracts! my-contracts)
  (f "a" 'b 'c))

(def input-failure-state (atom nil))
(def output-failure-state (atom nil))

(def your-contracts
  [{:var #'f
    :input-schema (schema/sequence-of [:or String clojure.lang.Keyword])
    :input-schema-on-failure (fn [f input errors]
                               (reset! input-failure-state [f input errors]))
    :output-schema String
    :output-schema-on-failure (fn [f result errors]
                                (reset! output-failure-state [f result errors]))}])

(deftest test-specifying-on-failure-handlers
  (add-contracts! your-contracts)

  (f "a" 'b 'c)
  (is (= [#'clj-schema.contracts-test/f
          ["a" 'b 'c]
          #{"Expected value c, at path [2], to be an instance of class java.lang.String, but was clojure.lang.Symbol"
            "Expected value b, at path [1], to be an instance of class java.lang.String, but was clojure.lang.Symbol"
            "Expected value c, at path [2], to be an instance of class clojure.lang.Keyword, but was clojure.lang.Symbol"
            "Expected value b, at path [1], to be an instance of class clojure.lang.Keyword, but was clojure.lang.Symbol"}]
         @input-failure-state))

  (f "a" "b" "c")
  (is (= [#'clj-schema.contracts-test/f
          :abc
          #{"Expected value :abc to be an instance of class java.lang.String, but was clojure.lang.Keyword"}]
         @output-failure-state))

  (remove-contracts! your-contracts))


(def input-success-state (atom nil))
(def output-success-state (atom nil))

(def oprahs-contracts
  [{:var #'f
    :input-schema (schema/sequence-of String)
    :input-schema-on-success (fn [f input]
                               (reset! input-success-state [f input]))
    :output-schema clojure.lang.Keyword
    :output-schema-on-success (fn [f result]
                                (reset! output-success-state [f result]))}])

(deftest test-specifying-on-success-handlers
  (add-contracts! oprahs-contracts)

  (f "a" "b" "c")
  (is (= [#'clj-schema.contracts-test/f ["a" "b" "c"]]
         @input-success-state))

  (f "a" "b" "c")
  (is (= [#'clj-schema.contracts-test/f :abc]
         @output-success-state))

  (remove-contracts! oprahs-contracts))

(def flaky-contracts
  [{:var #'f    
    :sampling-rate 50
    :input-schema (schema/sequence-of String)}])

(deftest test-contract-sampling-rate
  (add-contracts! flaky-contracts)
  (with-redefs [rand-int (constantly 49)]
    (is (thrown-with-msg? Exception #"Errors found in inputs"
          (f "a" 'b 'c))))

  (with-redefs [rand-int (constantly 50)]
    (is (thrown-with-msg? Exception #"Errors found in inputs"
          (f "a" 'b 'c))))

  (with-redefs [rand-int (constantly 51)]
    (f "a" 'b 'c)) ;; no exception!

  (remove-contracts! flaky-contracts))