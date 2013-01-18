(ns derp-octo-cyril.alternative
  (:refer-clojure :exclude [some empty]))

(defprotocol Alternative
  (empty [_])
  (combine [u v])
  (some [u])
  (many [u]))