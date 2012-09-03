(ns clj-schema.utils
  (:require [clojure.string :as str]))


;; TODO: Alex September 1, 2012: replace this whole file with runa.utils.xyz dependency

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
  (-> (str f)
      (str/replace #"(^clojure.core\$)|(_?@[0-9a-z]+$)" "")
      (str/replace #"\$" "/")
      (str/replace #"_QMARK" "?")
      (str/replace #"_" "-")))