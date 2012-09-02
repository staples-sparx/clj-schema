(ns clj-schema.fixtures-test
  (:use clj-schema.fixtures
        clojure.test)
  (:require [clj-schema.schema :as schema]))

(schema/def-validation-schema person-schema
  [[:name] String
   [:height] Integer])

(def fixture-1 (fixture person-schema {:name "Roberto"
                                       :height 555}))

(def-fixture fixture-2 person-schema {:name "Roberto"
                                      :height 555})

(def-fixture-factory a-factory person-schema
  [& {:keys [name height]
      :or {name "Roberto"
           height 555}}]
  {:name name
   :height height})

(def-fixture-factory b-factory person-schema
  ([]
    {:name "Roberto"
     :height 555})
  ([name]
    {:name name
     :height 555}))

(deftest test-fixture-goodness
  (testing "You can make fixtures in a variety of ways"
    (is (= {:name "Roberto"
            :height 555}
          fixture-1
          fixture-2
          (a-factory)
          (b-factory)))

    (is (= {:name "Foo"
            :height 111}
          (a-factory :name "Foo" :height 111)))

    (is (= {:name "Bar"
            :height 555}
          (b-factory "Bar"))))

  (testing "return values that don't pass validation throw AssertionErrors"
    (is (thrown? AssertionError (a-factory :name 555 :height "Bob")))
    (is (thrown? AssertionError (b-factory 555)))))