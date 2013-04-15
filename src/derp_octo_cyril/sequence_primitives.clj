(ns derp-octo-cyril.sequence-primitives
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.state :as s])
  (:require [derp-octo-cyril.error :as e]))

(extend-protocol p/ParserInput
  java.lang.String
  (parse [input parser]
    (p/run parser (s/state input)
           (fn [value _] value)
           (fn [value _] value)
           identity
           identity)))

(defn satisfy [pred]
  (reify p/Parser
    (run [_
          {:keys [input position] :as state}
          consumed-ok empty-ok consumed-error empty-error]
      (if (empty? input)
        (empty-error (e/unexpected "end of input" position))
        (let [x (first input)]
          (if (pred x)
            (let [new-position (s/update-position x position)
                  new-state (-> state
                                (assoc :position new-position)
                                (assoc :input (rest input)))]
              (consumed-ok x new-state))
            (empty-error (e/unexpected (str "'" x "'") position))))))))

(defn string [s]
  (let [n (count s)]
    (reify p/Parser
      (run [_
            {:keys [input position] :as state}
            consumed-ok empty-ok consumed-error empty-error]
        (if (< (count input) n)
          (empty-error (e/unexpected "end of input" position))
          (let [s' (reduce str "" (take n input))]
            (if (= s' s)
              (let [new-position (s/update-position s' position)
                    new-state (-> state
                                  (assoc :position new-position)
                                  (assoc :input (drop n input)))]
                (consumed-ok s' new-state))
              (empty-error (e/unexpected s' position)))))))))

(def any-char
  (p/label (satisfy (constantly true)) "any character"))

(defn char [c]
  (p/label (satisfy (fn [c'] (= c c'))) (str "'" c "'")))

(defn not-char [c]
  (p/label (satisfy (fn [c'] (not (= c c')))) (str "not '" c "'")))

(def alphanumeric
  (p/label (satisfy (fn [c] (or (Character/isLetter c)
                             (Character/isDigit c))))
           "alphanumeric"))

(def digit
  (p/label (satisfy (fn [c] (Character/isDigit c))) "digit"))

(def letter
  (p/label (satisfy (fn [c] (Character/isLetter c))) "letter"))

(def space
  (p/label (satisfy (fn [c] (= c \space))) "space"))

(defn one-of [cs]
  (p/label (satisfy (fn [c] (contains? cs c))) (str "one of " cs)))

(defn none-of [cs]
  (p/label (satisfy (fn [c] (not (contains? cs c)))) (str "none of " cs)))