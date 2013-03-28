(ns clj-schema.example-test
  (:use clj-schema.example
        clojure.test)
  (:require [clj-schema.schema :as schema]
            [clj-schema.simple-schemas :as ss]))

(schema/def-map-schema person-schema
  [[:name] ss/NonEmptyString
   [:height] Number])

(def example-1 (example person-schema {:name "Roberto"
                                       :height 555}))

(def-example example-2 person-schema {:name "Roberto"
                                      :height 555})

(def-example-factory a-factory person-schema
  [& {:keys [name height]
      :or {name "Roberto"
           height 555}}]
  {:name name
   :height height})

(def-example-factory b-factory person-schema
  ([]
    {:name "Roberto"
     :height 555})
  ([name]
    {:name name
     :height 555}))

(deftest test-example-goodness
  (testing "You can make examples in a variety of ways"
    (is (= {:name "Roberto"
            :height 555}
          example-1
          example-2
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
    (is (thrown-with-msg? AssertionError #"Was passed these keyword args" (a-factory :unspecified-key 777)))
    (is (thrown? AssertionError (b-factory 555)))))