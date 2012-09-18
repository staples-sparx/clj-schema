(ns clj-schema.optional-path-report-test
  (:use clojure.test
        clj-schema.optional-path-report)
  (:require [clj-schema.schema :as sch]))

(sch/defschema schema-w-optional-paths
  [(sch/optional-path [:a]) String
   (sch/optional-path [:b]) Number
   [:c] clojure.lang.Keyword])

(deftest test-optional-path-report
  (is (= {:all-optional-paths #{[:a] [:b]}
          :present #{[:a]}
          :absent #{[:b]}}

         (optional-path-report schema-w-optional-paths
                               {:a "hello"
                                :c :world}))))

(sch/defschema schema-with-no-optional-paths
  [[:a] String
   [:c] clojure.lang.Keyword])

(deftest test-no-optional-paths
  (is (= {:all-optional-paths #{}
          :present #{}
          :absent #{}}

         (optional-path-report schema-with-no-optional-paths
                               {:a "hello"
                                :c :world}))))