(defproject recursion "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [iloveponies.tests/recursion "0.1.0-SNAPSHOT"]
                 [proto-repl "0.3.1"]]
  :profiles {:dev {:plugins [[lein-midje "3.2.1"]]}})
