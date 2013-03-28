(ns architect.internal.utils
  (:require [clojure.set :as set]
            [clojure.string :as str]))


;;; general purpose utils

(defn map-nth [n f coll]
  (map-indexed (fn [idx x]
                 (if (zero? (rem (inc idx) n))
                   (f x)
                   x))
               coll))


;;; for blueprint.clj

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

;;; For simple_blueprints.clj

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

(defn single-destructuring-arg->form+name
  "Turns any one binding arg (which may be a destructuring binding) into a vector
   where the left elem is the arg with a possible :as added to it.
   And the rght side is the symbol referring to the arg itself."
  [arg-form]
  (let [as-symbol (gensym 'symbol-for-destructured-arg)
        snd-to-last-is-as? #(= :as (second (reverse %)))]
    (cond (and (vector? arg-form) (snd-to-last-is-as? arg-form))
          [arg-form (last arg-form)]

          (vector? arg-form)
          [(-> arg-form (conj :as) (conj as-symbol)) as-symbol]

          (and (map? arg-form) (contains? arg-form :as))
          [arg-form (:as arg-form)]

          (map? arg-form)
          [(assoc arg-form :as as-symbol) as-symbol]

          :else
          [arg-form arg-form])))

(defmacro defn-kw
  "A form of defn where the last arg is assumed to be keywords args, i.e.
   (defn-kw f [a b & {:keys [c d]}]
     (+ a b c d))
   Has built-in assertion that you have not accidentally passed in keys that
   were not listed in the key destructuring."
  [name arg-vec & body]
  (let [valid-key-set (if (map? (last arg-vec))
                        (set (map keyword (:keys (last arg-vec))))
                        #{})
        num-args (count arg-vec)
        [kw-destructuring kw-arg-map] (single-destructuring-arg->form+name (last arg-vec))
        new-arg-vec (vec (concat (drop-last 2 arg-vec) ['& kw-destructuring]))]
    (assert (map? (last arg-vec))
            "defn-kw expects the final element of the arg list to be a map destructuring.")
    (assert (contains? (last arg-vec) :keys)
            "defn-kw expects the map destructuring to have a :keys key.")
    (assert (= '& (last (butlast arg-vec)))
            "defn-kw expects the second to last element of the arg list to be an '&")
    `(defn ~name ~new-arg-vec
       (let [actual-key-set# (set (keys ~kw-arg-map))
             extra-keys# (set/difference actual-key-set# ~valid-key-set)]
         (when-not (empty? ~kw-arg-map)
           (assert (empty? extra-keys#)
                   (str "Was passed these keyword args " extra-keys#
                        " which were not listed in the arg list " '~arg-vec)))
         ~@body))))