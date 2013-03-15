(ns derp-octo-cyril.core
  (:refer-clojure :rename {empty core-empty
                           char core-char
                           sequence core-sequence})
  (:require [derp-octo-cyril.state :as s])
  (:require [derp-octo-cyril.error :as e]))

(defn consumed-ok [value state error]
  {:tag :consumed-ok
   :value value
   :state state
   :error error})

(defn empty-ok [value state error]
  {:tag :empty-ok
   :value value
   :state state
   :error error})

(defn consumed-error [error]
  {:tag :consumed-error
   :error error})

(defn empty-error [error]
  {:tag :empty-error
   :error error})

(def empty (fn [state] (empty-error (e/->unknown (:position state)))))

(defn char [c]
  (fn [{:keys [input position] :as state}]
    (if (empty? input)
      (empty-error (e/->sys-unexpected "" position))
      (let [[x & xs] input]
        (if (= c x)
          (let [new-position (s/update position x)]
            (consumed-ok x
                         (-> state
                             (assoc :input xs)
                             (assoc :position new-position))
                         (e/->unknown new-position)))
          (empty-error (e/->sys-unexpected (str c) position)))))))

(defn ^{:private true}
  sequence-c-ok
  [{f :value error :error}
   {tag :tag x :value state :state error' :error :as result}]
  (case tag
    :consumed-ok (consumed-ok (f x) state error')
    :empty-ok (consumed-ok (f x) state (e/concat error error'))
    :consumed-error result
    :empty-error (consumed-error state (e/concat error error'))))

(defn ^{:private true}
  sequence-e-ok
  [{f :value error :error}
   {tag :tag x :value state :state error' :error :as result}]
  (case tag
    :consumed-ok (consumed-ok (f x) state error')
    :empty-ok (empty-ok (f x) state (e/concat error error'))
    :consumed-error result
    :empty-error (empty-error state (e/concat error error'))))

(defn sequence [p q]
  (fn [state]
    (let [{:keys [tag state] :as result} (p state)]
      (case tag
        :consumed-ok (sequence-c-ok result (q state))
        :empty-ok (sequence-e-ok result (q state))
        :consumed-error result
        :empty-error result))))

(defn ^{:private true}
  merge-error
  [result error]
  (update-in result [:error] (fn [error'] (e/concat error error'))))

(defn choose [p q]
  (fn [state]
    (let [{:keys [tag state error] :as result} (p state)]
      (case tag
        :consumed-ok result
        :empty-ok result
        :consumed-error result
        :empty-error (let [{:keys [tag] :as result} (q state)]
                       (case tag
                         :consumed-ok result
                         :empty-ok (merge-error result error)
                         :consumed-error result
                         :empty-error (merge-error result error)))))))

(defn -main [& args]
  (println ((choose (char \x) (char \y)) (s/->state [\x]))))