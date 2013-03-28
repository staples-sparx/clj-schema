(ns ^{:doc "Common simple blueprints shared within validation blueprints."}
  architect.simple-blueprints
  (:use [architect.blueprint :only [def-simple-blueprint simple-blueprint]])
  (:require [architect.internal.utils :as u])
  (:import clojure.lang.Keyword))


(def-simple-blueprint Anything (constantly true))

(def-simple-blueprint FloatingPoint      [:or Float Double])
(def-simple-blueprint Integral           [:or Long Integer])
(def-simple-blueprint NonNegFloatingPoint [FloatingPoint (complement neg?)])
(def-simple-blueprint PosFloatingPoint    [FloatingPoint pos?])
(def-simple-blueprint Percentage         [NonNegFloatingPoint #(<= % 100.0)])
(def-simple-blueprint SchemaVersionNum   [Integral pos?])
(def-simple-blueprint TimeStamp          [Integral u/timestamp?])
(def-simple-blueprint NonNegIntegral     [Integral u/non-neg-integer?])
(def-simple-blueprint NonPosIntegral     [Integral (complement pos?)])
(def-simple-blueprint PosIntegral        [Integral pos?])

(def-simple-blueprint Real               [:or Integral Float Double clojure.lang.Ratio])
(def-simple-blueprint Probability        [Real #(and (<= 0 %) (<= % 1))])

(def-simple-blueprint NonEmptyString [String u/non-empty?])
(def-simple-blueprint UUID           [String u/uuid?])
(def-simple-blueprint GitSha         [String u/git-sha?])
(def-simple-blueprint URL            [String u/url?])
(def-simple-blueprint IPAddress      [String u/ip-address-v4?])

(def ^{:private true} can-parse-boolean? (comp (constantly true) u/parse-boolean))

(def-simple-blueprint StringTimeStamp [String (comp u/timestamp? u/parse-long)])
(def-simple-blueprint StringDecimal   [String u/parse-double])
(def-simple-blueprint StringIntegral  [String [:or u/parse-integer u/parse-long]])
(def-simple-blueprint StringBoolean   [String can-parse-boolean?])

(def-simple-blueprint KeywordizedUUIDAndTimestamp [Keyword (comp u/uuid+timestamp? name)])
(def-simple-blueprint KeywordizedUUID [Keyword (comp u/uuid? name)])

(defn OneOf [& values]
  (if (and (set? (first values))
           (= 1 (count values)))
    ;; just for convenience if you want to say (OneOf #{:a :b :c})
    (simple-blueprint [(class (ffirst values)) (first values)])
    (simple-blueprint [(class (first values)) (set values)])))
