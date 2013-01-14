(defproject derp-octo-cyril "0.1.0-SNAPSHOT"
  :description ""
  :url "https://github.com/rapala/derp-octo-cyril"
  :license {:name "ISC license"
            :url "http://www.isc.org/software/license"}
  :repositories {"stuartsierra-releases" "http://stuartsierra.com/maven2"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[midje "1.4.0"]
                                  [com.stuartsierra/lazytest "1.2.3"]]}}
  :warn-on-reflection true
  :main derp-octo-cyril.core)