(ns derp-octo-cyril.state)

(defprotocol Position
  (update [self char]))

(deftype APosition [name line column]
  Position
  (update [_ x]
    (if (= x \newline)
      (APosition. name (inc line) 0)
      (APosition. name line (inc column))))
  Object
  (toString [_]
    (str (if name (str "\"" name "\""))
         "(line " line ", column " column ")")))

(defn ->position
  ([line column] (->position nil line column))
  ([name line column] (->APosition name line column)))

(defmethod clojure.core/print-method Position
  [position writer]
  (.write writer (str position)))

(defrecord State [input position user])

(defn ->state
  ([input] (->state input (->position 0 0) nil))
  ([input position] (->state input position nil))
  ([input position user] (->State input position user)))