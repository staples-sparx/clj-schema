(ns clj-schema.utils-test
  (:use clojure.test
        clj-schema.utils))


(deftest test-paths
  (are [m result] (= (paths m) result)
       nil            nil
       {:a 1}         '([:a])
       {:a 1
        :b 2}         '([:a] [:b])
       {:a 1
        :b {:c 2
            :d 3}}    '([:a] [:b :c] [:b :d])))

(deftest test-subpaths
  (are [path sps] (= (subpaths path) sps)
    nil        []
    [:a]       [[:a]]
    [:a :b :c] [[:a] [:a :b] [:a :b :c]]))

(deftest test-subpath?
  (are [root-path path result] (= (subpath? root-path path) result)
       nil nil false
       [:a] [:a] true
       [:a] [:a :b] true
       [:a] [:a :b :c] true
       [:a :b] [:c] false
       [:a :b] [:a] false
       [:a :b] [:a :b] true
       [:a :b] [:a :e :c] false))

(deftest test-fn->fn-thats-false-if-throws
  (are [f called-with result] (= ((fn->fn-thats-false-if-throws f) called-with) result)
    string? "s" true
    string? 99 false
    pos? "by itself, pos? would blow up given non-number" false))

(defn foo? [] :foo)

(deftest test-pretty-fn-str
  (testing "fully qualified fn names"
    (are [f name] (= (pretty-fn-str f) name)
      nil nil
      paths "clj-schema.utils/paths" ;; fn names are fully namespace-qualifed
      foo? "clj-schema.utils-test/foo?"
      integer? "integer?" ;; except clojure.core fns, which aren't
      )))

(deftest test-non-neg-integer?
  (are [x result] (= (non-neg-integer? x) result)
    -4444444 false
    -333333  false
    -10000   false
    -999     false
    -77      false
    -5       false
    -3       false
    -2       false
    -1       false
    0        true
    1        true
    2        true
    3        true
    5        true
    77       true
    999      true
    10000    true
    333333   true
    4444444  true ))

(deftest test-url?
   (are [s result] (= (url? s) result)
     nil false
     ""  false
     "http://www.amazon.com" true))

(deftest test-ip-address-v4?
  (are [s result ] (= (ip-address-v4? s) result)
    nil false
    ""  false
    "192.168.10.255" true))

(deftest test-timestamp?
  (are [n result] (= (timestamp? n) result)
    nil      false
    -4444444 false
    -333333  false
    -10000   false
    -999     false
    -77      false
    -5       false
    -3       false
    -2       false
    -1       false
    0        true
    1        true
    2        true
    3        true
    5        true
    77       true
    999      true
    10000    true
    333333   true
    4444444  true
    0.0        false
    1.0        false
    2.0        false
    3.0        false
    5.0        false
    77.0       false
    999.0      false
    10000.0    false
    333333.0   false
    4444444.0  false
    9223372036854775807 true
    9223372036854775808 false))  ;; past the max long value
