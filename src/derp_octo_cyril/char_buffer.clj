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

(defn char [c]
  (p/label (satisfy (fn [c'] (.equals c c'))) (str c)))

(def letter
  (p/label (satisfy (fn [c] (Character/isLetter c))) "letter"))