(ns derp-octo-cyril.state)

(defrecord Position [name line column]
  Comparable
  (compareTo [_ {line' :line column' :column}]
    (cond (< line line') -1
          (= line line') (cond (< column column') -1
                               (= column column') 0
                               :else 1)
          :else 1))
  Object
  (toString [_]
    (str (if name (str name ": "))
         "(" line ", " column ")")))

(defn position
  ([line column] (position nil line column))
  ([name line column] (Position. name line column)))

(defprotocol Token
  (update-position [token position]))

(defn ^{:private true}
  newline? [c]
  (= c \newline))

(extend-protocol Token
  java.lang.Character
  (update-position [c position]
    (if (newline? c)
      (-> position
          (update-in [:line] inc)
          (assoc :column 0))
      (update-in position [:column] inc)))
  java.lang.String
  (update-position [s position]
    (let [newlines (filter newline? s)
          last-line (take-while (complement newline?) (reverse s))]
      (-> position
          (update-in [:line] + (count newlines))
          (update-in [:column] + (count last-line))))))

(defrecord State [input position user])

(defn state
  ([input] (state input (position 0 0) nil))
  ([input position] (state input position nil))
  ([input position user] (State. input position user)))