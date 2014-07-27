(defproject ghostasm "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.match "0.2.1"]
                 [prismatic/plumbing "0.3.3"]]
  :main ghostasm.core
  :profiles {:uberjar {:aot :all}})
