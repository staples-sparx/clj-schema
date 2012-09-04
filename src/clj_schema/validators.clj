(ns ^{:doc "Common validators shared within validation schemas."}
  clj-schema.validators
  (:require [clj-schema.schema :as vs]
            [clj-schema.utils :as u])
  (:import clojure.lang.Keyword))


;; All of these validators are using Pascal case to indicate there
;; is a Class object associated with them

(def FloatingPoint      [:or Float Double])
(def Integral           [:or Long Integer])
(def NonNegFloatingPoint [FloatingPoint (complement neg?)])
(def PosFloatingPoint    [FloatingPoint pos?])
(def Percentage         [NonNegFloatingPoint #(<= % 100.0)])
(def SchemaVersionNum   [Integral pos?])
(def TimeStamp          [Integral u/timestamp?])
(def NonNegIntegral     [Integral u/non-neg-integer?])
(def NonPosIntegral     [Integral (complement pos?)])
(def PosIntegral        [Integral pos?])

(def Real               [:or Integral Float Double clojure.lang.Ratio])
(def Probability        [Real #(and (<= 0 %) (<= % 1))])

(def NonEmptyString [String u/non-empty?])
(def UUID           [String u/uuid?])
(def GitSha         [String u/git-sha?])
(def URL            [String u/url?])
(def IPAddress      [String u/ip-address-v4?])

(def ^{:private true} can-parse-boolean? (comp (constantly true) u/parse-boolean))

(def StringTimeStamp [String (comp u/timestamp? u/parse-long)])
(def StringDecimal   [String u/parse-double])
(def StringIntegral  [String [:or u/parse-integer u/parse-long]])
(def StringBoolean   [String can-parse-boolean?])

(def KeywordizedUUIDAndTimestamp [Keyword (comp u/uuid+timestamp? name)])
(def KeywordizedUUID [Keyword (comp u/uuid? name)])

(defn OneOf [& values]
  (if (and (set? (first values))
           (= 1 (count values)))
    ;; just for convenience if you want to say (OneOf #{:a :b :c})
    [(class (ffirst values)) (first values)]
    [(class (first values)) (set values)]))

