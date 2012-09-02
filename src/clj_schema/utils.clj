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

(def fns-w-out-name-in-meta->name
  {string? "string?"
   first "first"
   second "second"
   map?   "map?"})

(defn verbose-fn-name
  "string?         ;=> 'string?'
   my-fn           ;=> 'my.namespace/my-fn'
   #(integer? %)   ;=> '(fn* [p1__150085#] (integer? p1__150085#))'
   NOTE: won't work past Clojure 1.2"
  [f]
  (when f
    (if-let [special-case-fn (fns-w-out-name-in-meta->name f)]
      special-case-fn
      (let [{:keys [^clojure.lang.Namespace ns name]} (meta f)]
        (if-not (and ns name)
          (str f)
          (let [ns-name (str/trim (.getName ns))
                fn-name (str/trim name)]
            (if (= "clojure.core" ns-name)
              fn-name
              (str ns-name "/" fn-name))))))))