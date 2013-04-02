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
  (with-redefs [rand (constantly 49.999)]
    (is (thrown-with-msg? Exception #"Errors found in inputs"
          (f "a" 'b 'c))))

  (with-redefs [rand (constantly 50.0)]
    (f "a" 'b 'c))

  (remove-contracts! flaky-contracts))

(defn doubler [n]
  (* 2 n))

(def fn-dependent-contracts
  [{:var #'doubler
    :sampling-rate (fn [n] n)
    :output-schema String}])

(deftest test-contract-with-function-sampling-rate
  (add-contracts! fn-dependent-contracts)

  (with-redefs [rand (constantly 49.999)]
    (is (thrown-with-msg? Exception #"Errors found in outputs"
           (doubler 50))))

  (with-redefs [rand (constantly 50.0)]
    (doubler 50)) ;; no exception!

  ;; Lower-bound edge case
  (with-redefs [rand (constantly 0.0)]
    (doubler 0))

  ;; Upper-bound edge case
  (with-redefs [rand (constantly 100.0)]
    (doubler 100))

  (remove-contracts! fn-dependent-contracts))