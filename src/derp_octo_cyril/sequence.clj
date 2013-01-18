(ns derp-octo-cyril.sequence
  (:refer-clojure :exclude [take-while])
  (:require [derp-octo-cyril.polymorphism-parser :as pi])
  (:require [derp-octo-cyril.applicative :as ap])
  (:require [derp-octo-cyril.alternative :as al])
  (:require [derp-octo-cyril.monad :as m])
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.error :as e])
  (:require [derp-octo-cyril.state :as s]))

(defn one [pred]
  (pi/->Parser
   (fn [{:keys [state consumed-ok empty-error]}]
     (let [{:keys [input position user]} state
           x (first input)]
       (cond (empty? input)
             (empty-error {:error (e/->sys-unexpected "" position)})
             (pred x)
             (let [new-position (s/inc-column position)
                   new-state (s/->State (rest input) new-position user)]
               (consumed-ok {:x x
                             :state new-state
                             :error (e/->unknown new-position)}))
             :else
             (empty-error
              {:error (e/->sys-unexpected (pr-str x) position)}))))))

(defn take-while [pred]
  (pi/->Parser
   (fn [{:keys [state consumed-ok empty-ok]}]
     (let [{:keys [input position user]} state]
       (let [[satisfied dropped] (split-with pred input)
             cont (if (empty? satisfied) empty-ok consumed-ok)
             position (loop [p position
                             t (count satisfied)]
                        (if (zero? t) p (recur (s/inc-column p) (dec t))))]
         (cont {:x satisfied
                :state (s/->State dropped position user)
                :error (e/->unknown position)}))))))

(defn character [c]
  (p/label (one (partial = c))
           (str "'" c "'")))

(def letter
  (p/label (one (fn [^Character c] (Character/isLetter c)))
           "letter"))

(def digit
  (p/label (one (fn [^Character c] (Character/isDigit c)))
           "digit"))

(def decimal
  (let [digits-parser (take-while (fn [^Character c] (Character/isDigit c)))]
    (p/label (m/bind digits-parser
                     (fn [s]
                       (if (empty? s)
                         (al/empty digits-parser)
                         (ap/pure digits-parser
                                  (Integer/parseInt (reduce str "" s))))))
             "decimal")))