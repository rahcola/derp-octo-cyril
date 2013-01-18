(ns derp-octo-cyril.monad)

(defprotocol Monad
  (bind [this f]))