(ns ^{:doc "Common simple schemas shared within validation schemas."}
  clj-schema.simple-schemas
  (:use [clj-schema.schema :only [and-statement-schema
                                  class-schema
                                  def-simple-schema
                                  predicate-schema
                                  simple-schema]])
  (:require [clj-schema.internal.utils :as u])
  (:import clojure.lang.Keyword))


(def-simple-schema Anything (constantly true))

(def-simple-schema FloatingPoint      [:or Float Double])
(def-simple-schema Integral           [:or Long Integer])
(def-simple-schema NonNegFloatingPoint [FloatingPoint (complement neg?)])
(def-simple-schema PosFloatingPoint    [FloatingPoint pos?])
(def-simple-schema Percentage         [NonNegFloatingPoint #(<= % 100.0)])
(def-simple-schema SchemaVersionNum   [Integral pos?])
(def-simple-schema TimeStamp          [Integral u/timestamp?])
(def-simple-schema NonNegIntegral     [Integral u/non-neg-integer?])
(def-simple-schema NonPosIntegral     [Integral (complement pos?)])
(def-simple-schema PosIntegral        [Integral pos?])

(def-simple-schema Real               [:or Integral Float Double clojure.lang.Ratio])
(def-simple-schema Probability        [Real #(and (<= 0 %) (<= % 1))])

(def-simple-schema NonEmptyString [String u/non-empty?])
(def-simple-schema UUID           [String u/uuid?])
(def-simple-schema GitSha         [String u/git-sha?])
(def-simple-schema URL            [String u/url?])
(def-simple-schema IPAddress      [String u/ip-address-v4?])

(def ^{:private true} can-parse-boolean? (comp (constantly true) u/parse-boolean))

(def-simple-schema StringTimeStamp [String (comp u/timestamp? u/parse-long)])
(def-simple-schema StringDecimal   [String u/parse-double])
(def-simple-schema StringIntegral  [String [:or u/parse-integer u/parse-long]])
(def-simple-schema StringBoolean   [String can-parse-boolean?])

(def-simple-schema KeywordizedUUIDAndTimestamp [Keyword (comp u/uuid+timestamp? name)])
(def-simple-schema KeywordizedUUID [Keyword (comp u/uuid? name)])

(defn OneOf [& values]
  (if (and (set? (first values))
           (= 1 (count values)))
    ;; just for convenience if you want to say (OneOf #{:a :b :c})
    (and-statement-schema [(class-schema (class (ffirst values))) (simple-schema (first values))])
    (and-statement-schema [(class-schema (class (first values))) (predicate-schema (set values))])))
