(def common-deps '[]) ;; if any ever exist

(defproject org.clojars.runa/clj-schema "0.6.4"
  :description "Map schema definitions and validation library."
  :dependencies ~(cons '[org.clojure/clojure "1.2.0"]
                         common-deps)
  :dev-dependencies [[jonase/kibit "0.0.3"]
                     [jonase/eastwood "0.0.2"]
                     [lein-multi "1.1.0"]]
  :multi-deps {"1.2.0" [[org.clojure/clojure "1.2.0"]]
               "1.2.1" [[org.clojure/clojure "1.2.1"]]
               "1.3.0" [[org.clojure/clojure "1.3.0"]]
               "1.4.0" [[org.clojure/clojure "1.4.0"]]
               "1.5.0" [[org.clojure/clojure "1.5.0-alpha3"]]
                :all ~common-deps})