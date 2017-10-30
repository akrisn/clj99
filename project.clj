(defproject clj99 "0.1.0-SNAPSHOT"
  :description "Solve L99."
  :url "https://github.com/stn/clj99"
  :license {:name "MIT License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot clj99.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
