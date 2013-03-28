(defproject org.clojars.runa/architect "1.0.0-SNAPSHOT"
  :min-lein-version "2.0.0"
  :description "Blueprints for validating Clojure data, and making valid example data"
  :url "https://github.com/runa-dev/architect"
  :license {:name "MIT License"
            :url "http://mit-license.org/"}
  :profiles {:user {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :dev {:dependencies [[gui-diff "0.5.0"]
                                  [jonase/kibit "0.0.3"]
                                  [slamhound "1.3.0"]
                                  [org.clojure/tools.trace "0.7.3"]]
                   :plugins [[codox "0.6.4"]
                             [jonase/eastwood "0.0.2"]]}
             :1.3.0 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4.0 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5.0 {:dependencies [[org.clojure/clojure "1.5.0-RC4"]]}}
  :aliases {"run-tests" ["with-profile" "1.3.0:1.4.0:1.5.0" "test"]
            "slamhound" ["run" "-m" "slam.hound"]})
