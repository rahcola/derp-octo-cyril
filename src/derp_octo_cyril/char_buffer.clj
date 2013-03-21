(ns derp-octo-cyril.char-buffer
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.error :as e])
  (:require [derp-octo-cyril.state :as s])
  (:import java.nio.CharBuffer)
  (:import java.nio.charset.StandardCharsets))

(extend-protocol p/ParserInput
  java.lang.String
  (parse [input parser]
    (p/run parser (s/state (CharBuffer/wrap input))
           (fn [value _ _] value)
           (fn [value _ _] value)
           identity
           identity)))

(defn satisfy [pred]
  (reify p/Parser
    (run [_
          {:keys [^CharBuffer input position] :as state}
          consumed-ok empty-ok consumed-error empty-error]
      (if (.hasRemaining input)
        (let [x (do (.mark input)
                    (.get input))]
          (if (pred x)
            (let [new-position (s/update position x)
                  new-state (assoc state :position new-position)]
              (consumed-ok x new-state (e/unknown new-position)))
            (do (.reset input)
                (empty-error (e/sys-unexpected (str x) position)))))
        (empty-error (e/sys-unexpected "" position))))))

(defn string [s]
  (let [n (.length s)]
    (reify p/Parser
      (run [_
            {:keys [^CharBuffer input position] :as state}
            consumed-ok empty-ok consumed-error empty-error]
        (if (< (.remaining input) n)
          (empty-error (e/sys-unexpected "" position))
          (let [arr (char-array n)
                s' (do (.mark input) (.get input arr) (String. arr))]
            (if (= s' s)
              (let [new-position (s/update position s')
                    new-state (assoc state :position new-position)]
                (consumed-ok s' new-state (e/unknown new-position)))
              (do (.reset input)
                  (empty-error (e/sys-unexpected s' position))))))))))

(def any-char
  (p/label (satisfy (constantly true)) "any character"))

(defn char [c]
  (p/label (satisfy (fn [c'] (.equals c c'))) (str c)))

(defn not-char [c]
  (p/label (satisfy (fn [c'] (not (.equals c c')))) (str "not " c)))

(def alphanumeric
  (p/label (satisfy (fn [c] (or (Character/isLetter c)
                               (Character/isDigit c))))
           "alphanumeric"))

(def digit
  (p/label (satisfy (fn [c] (Character/isDigit c))) "digit"))

(def letter
  (p/label (satisfy (fn [c] (Character/isLetter c))) "letter"))

(defn one-of [cs]
  (p/label (satisfy (fn [c] (contains? cs c))) (str "one of " cs)))

(defn none-of [cs]
  (p/label (satisfy (fn [c] (not (contains? cs c)))) (str "none of " cs)))