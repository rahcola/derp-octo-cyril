(ns derp-octo-cyril.state)

(defprotocol Position
  (inc-column [position n])
  (inc-line [position n]))

(deftype APosition [name line column]
  Position
  (inc-column [_ n]
    (APosition. name line (+ column n)))
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
  (update-position [token position]))

(defn ^{:private true}
  newline? [c]
  (= c \newline))

(extend-protocol Token
  java.lang.Character
  (update-position [c position]
    (if (newline? c)
      (inc-line position 1)
      (inc-column position 1)))
  java.lang.String
  (update-position [s position]
    (let [newlines (filter newline? s)
          last-line (take-while (complement newline?) (reverse s))]
      (-> (inc-line position (count newlines))
          (inc-column (count last-line))))))

(defn state
  ([input]
     {:input input
      :position (position 0 0)})
  ([input position]
     {:input input
      :position position})
  ([input position user]
     {:input input
      :position position
      :user user}))
