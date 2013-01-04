(defproject org.clojars.runa/clj-schema "0.8.2"
  :min-lein-version "2.0.0"
  :description "Map schema definitions and validation library."
  :url "https://github.com/runa-dev/clj-schema"
  :license {:name "MIT License"
            :url "http://mit-license.org/"}
  :profiles {:user {:dependencies [[org.clojure/clojure "1.2.1"]]}
             :dev {:dependencies [[jonase/kibit "0.0.3"]
                                  [jonase/eastwood "0.0.2"]
                                  [slamhound "1.3.0"]
                                  [org.clojure/tools.trace "0.7.3"]]}
             :1.2.1 {:dependencies [[org.clojure/clojure "1.2.1"]]}
             :1.3.0 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4.0 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5.0 {:dependencies [[org.clojure/clojure "1.5.0-RC1"]]}}
  :aliases {"slamhound" ["run" "-m" "slam.hound"]})
