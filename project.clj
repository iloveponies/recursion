(defproject recursion "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.trace "0.7.5"]
                 [iloveponies.tests/recursion "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:plugins [[lein-midje "3.1.1"]]}})
