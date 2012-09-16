(ns clj-schema.internal.utils
  (:require [clojure.string :as str]))


;; TODO: Alex September 1, 2012: replace this whole file with runa.utils.xyz dependency

;;; for schema.clj

(defn paths
  "Return the paths of the leaves in the map"
  [m]
  (when m
    (letfn [(key-paths [prefix m]
              (if (map? m)
                (into {} (map (fn [[k v]] (key-paths (conj prefix k) v)) m))
                {prefix m}))]
      (keys (key-paths [] m)))))

(defn subpaths [path]
  (rest (reductions conj [] path)))

(defn subpath?
  "true if 'path' is a child of 'root-path'"
  [root-path path]
  (= root-path
    (take (count root-path) path)))

(defn fn->fn-thats-false-if-throws
  "Takes a fn.  Returns a new fn that evaluates
   to false if it throws an Exception."
  [f]
  (fn [& args]
    (try
      (apply f args)
      (catch Throwable e false))))

(defn pretty-fn-str [f]
  (when f
    (-> (str f)
        (str/replace #"(^clojure.core\$)|(_?@[0-9a-z]+$)" "")
        (str/replace #"\$" "/")
        (str/replace #"_QMARK" "?")
        (str/replace #"_" "-"))))

;;; For validators.clj

(defn parse-long [s]
  (Long/parseLong s))

(defn parse-integer [s]
  (Integer/parseInt s))

(defn parse-boolean [s]
  (Boolean/parseBoolean s))

(defn parse-double [s]
  (Double/parseDouble s))

(defn timestamp? [n]
  (and (integer? n)
       (>= n 0)
       (<= n Long/MAX_VALUE)))

(defn non-neg-integer? [x]
  (and (integer? x)
       (not (neg? x))))

(defn git-sha? [s]
  (if s
    (boolean (re-matches #"([a-f]|\d){40}" s))
    false))

(defn url
  "Returns a java.net.URL instance or nil if URL failed to parse"
  [^String s]
  (when s
    (try
      (java.net.URL. s)
      (catch java.net.MalformedURLException e
        nil))))

(defn url? [s]
  (boolean (url s)))

(def ^{:private true} valid-ip-address-v4-re
  #"^([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])\.([01]?\d\d?|2[0-4]\d|25[0-5])$")

(defn ip-address-v4? [s]
  (boolean (when s
             (re-matches valid-ip-address-v4-re s))))

(defn non-empty? [x]
  (not (empty? x)))

(defn uuid? [s]
  (if s
    (boolean (re-matches #"[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}" s))
    false))

(defn uuid+timestamp? [s]
  (if s
    (boolean (re-matches #"[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}-\d{13}" s))
    false))

