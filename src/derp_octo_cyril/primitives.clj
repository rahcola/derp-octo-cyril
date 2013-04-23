(ns derp-octo-cyril.primitives
  (:refer-clojure :rename {char core-char})
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.combinators :as c])
  (:require [derp-octo-cyril.state :as s])
  (:require [derp-octo-cyril.error :as e]))

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
  (c/label (satisfy (constantly true)) "any character"))

(defn char [c]
  (c/label (satisfy (fn [c'] (= c c'))) (str "'" c "'")))

(defn not-char [c]
  (c/label (satisfy (fn [c'] (not (= c c')))) (str "not '" c "'")))

(def alphanumeric
  (c/label (satisfy (fn [c] (or (Character/isLetter c)
                             (Character/isDigit c))))
           "alphanumeric"))

(def digit
  (c/label (satisfy (fn [c] (Character/isDigit c))) "digit"))

(def letter
  (c/label (satisfy (fn [c] (Character/isLetter c))) "letter"))

(def whitespace
  (c/label (satisfy (fn [c] (Character/isWhitespace c))) "whitespace"))

(def space
  (c/label (satisfy (fn [c] (= c \space))) "space"))

(defn one-of [cs]
  (c/label (satisfy (fn [c] (contains? cs c))) (str "one of " cs)))

(defn none-of [cs]
  (c/label (satisfy (fn [c] (not (contains? cs c)))) (str "none of " cs)))