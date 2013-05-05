(ns derp-octo-cyril.primitives
  (:refer-clojure :rename {char core-char})
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.combinators :as c])
  (:require [derp-octo-cyril.state :as s])
  (:require [derp-octo-cyril.error :as e]))

(defn update-state [state input position]
  (-> (assoc state :input input)
      (assoc :position position)))

(defn satisfy [extract satifies? show]
  (reify p/Parser
    (run [_
          {:keys [input position] :as state}
          consumed-ok empty-ok consumed-error empty-error]
      (if (empty? input)
        #(empty-error (e/unexpected "end of input" position))
        (let [[x rest] (extract input)]
          (if (satifies? x)
            #(consumed-ok x (update-state state
                                          rest
                                          (s/advance-position x position)))
            #(empty-error (e/unexpected (show x) position))))))))

(defn char-satisfy [satifies?]
  (satisfy (juxt first rest)
           satifies?
           (fn [c] (str "'" c "'"))))

(defn string [s]
  (let [n (count s)]
    (satisfy (juxt (comp (partial reduce str "")
                         (partial take n))
                   (partial drop n))
             (partial = s)
             identity)))

(def any-char
  (c/label (char-satisfy (constantly true)) "any character"))

(defn char [c]
  (c/label (char-satisfy (fn [c'] (= c c'))) (str "'" c "'")))

(defn not-char [c]
  (c/label (char-satisfy (fn [c'] (not (= c c')))) (str "not '" c "'")))

(def alphanumeric
  (c/label (char-satisfy (fn [c] (or (Character/isLetter c)
                             (Character/isDigit c))))
           "alphanumeric"))

(def digit
  (c/label (char-satisfy (fn [c] (Character/isDigit c))) "digit"))

(def letter
  (c/label (char-satisfy (fn [c] (Character/isLetter c))) "letter"))

(def whitespace
  (c/label (char-satisfy (fn [c] (Character/isWhitespace c))) "whitespace"))

(def space
  (c/label (char-satisfy (fn [c] (= c \space))) "space"))

(defn one-of [cs]
  (c/label (char-satisfy (fn [c] (contains? cs c))) (str "one of " cs)))

(defn none-of [cs]
  (c/label (char-satisfy (fn [c] (not (contains? cs c)))) (str "none of " cs)))
