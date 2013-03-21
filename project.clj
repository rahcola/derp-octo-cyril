(defproject derp-octo-cyril "0.1.0-SNAPSHOT"
  :description ""
  :url "https://github.com/rapala/derp-octo-cyril"
  :license {:name "ISC license"
            :url "http://www.isc.org/software/license"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.clojure/algo.generic "0.1.1"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}}
  :main derp-octo-cyril.core)