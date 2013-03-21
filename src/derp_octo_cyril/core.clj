(ns derp-octo-cyril.core
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.char-buffer :as cb]))

(def whitespace (p/some (p/choose (cb/char \space)
                                  (cb/char \newline))))

(def atom (p/lift (comp symbol (partial apply str))
                  (p/some cb/letter)))

(def list
  (delay (p/lift (fn [_ atoms _] atoms)
                 (cb/char \()
                 (p/some-separated (p/choose atom list) whitespace)
                 (cb/char \)))))

(defn -main [& args])