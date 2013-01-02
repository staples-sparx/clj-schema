(def common-deps '[]) ;; if any ever exist

(defproject org.clojars.runa/clj-schema "0.8.2"
  :description "Map schema definitions and validation library."
  :url "https://github.com/runa-dev/clj-schema"
  :license {:name "MIT License"
            :url "http://mit-license.org/"}
  :dependencies ~(cons '[org.clojure/clojure "1.2.1"]
                         common-deps)
  :dev-dependencies [[jonase/kibit "0.0.3"]
                     [jonase/eastwood "0.0.2"]
                     [slamhound "1.3.0"]
                     [lein-multi "1.1.0"]
                     [org.clojure/tools.trace "0.7.3"]]
  :profiles {:dev {:dependencies [[jonase/kibit "0.0.3"]
                                  [jonase/eastwood "0.0.2"]
                                  [slamhound "1.3.0"]]}}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]}
  :multi-deps {"1.2.1" [[org.clojure/clojure "1.2.1"]]
               "1.3.0" [[org.clojure/clojure "1.3.0"]]
               "1.4.0" [[org.clojure/clojure "1.4.0"]]
               "1.5.0" [[org.clojure/clojure "1.5.0-RC1"]]
                :all ~common-deps})
