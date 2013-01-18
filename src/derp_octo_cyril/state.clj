(ns derp-octo-cyril.state)

(defprotocol APosition
  (inc-line [this])
  (inc-column [this]))

(defrecord Position [line column]
  APosition
  (inc-line [this]
    (->Position (inc line) column))
  (inc-column [this]
    (->Position line (inc column))))

(defrecord State [input position user])