(ns derp-octo-cyril.state)

(defprotocol Position
  (inc-line [position n]))

(defrecord APosition [name line column]
  Position
  (inc-line [_ n]
    (APosition. name (+ line n) 0))
  Object
  (toString [_]
    (str (if name (str name ", "))
         "(" line "," column ")")))

(defn position
  ([line column]
     (position nil line column))
  ([name line column]
     (APosition. name line column)))

(defprotocol Token
  (advance-position [token position]))

(defn ^{:private true}
  newline? [c]
  (= c \newline))

(extend-protocol Token
  java.lang.Character
  (advance-position [c position]
    (if (newline? c)
      (inc-line position 1)
      (update-in position [:column] inc)))
  java.lang.String
  (advance-position [s position]
    (let [newlines (filter newline? s)
          last-line (take-while (complement newline?) (reverse s))]
      (-> (inc-line position (count newlines))
          (assoc :column (count last-line))))))

(defrecord AState [input position])

(defn state
  ([input]
     (state input (position 0 0)))
  ([input position]
     (AState. input position)))
