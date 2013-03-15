(ns derp-octo-cyril.core
  (:refer-clojure :rename {empty core-empty
                           some core-some
                           char core-char
                           sequence core-sequence})
  (:require [derp-octo-cyril.state :as s])
  (:require [derp-octo-cyril.error :as e])
  (:import java.nio.CharBuffer))

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

(defn satisfy [p]
  (fn [{:keys [^CharBuffer input position] :as state}]
    (if (.hasRemaining input)
      (let [x (do (.mark input)
                  (.get input))]
        (if (p x)
          (let [new-position (s/update position x)
                new-state (-> state
                             (assoc :input input)
                             (assoc :position new-position))]
            (consumed-ok x new-state (e/->unknown new-position)))
          (do (.reset input)
              (empty-error (e/->sys-unexpected (str x) position)))))
      (empty-error (e/->sys-unexpected "" position)))))

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
    (let [{:keys [tag error] :as result} (p state)]
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

(defn many [p]
  (let [empty-many (RuntimeException. "many applied to an empty parser")
        walk (fn [acc state {state' :state :keys [tag value error]}]
               (case tag
                 :consumed-ok (recur (conj acc value)
                                     state'
                                     (p state'))
                 :empty-ok (throw empty-many)
                 :consumed-error (consumed-error error)
                 :empty-error (consumed-ok acc state error)))]
    (fn [state]
      (let [{:keys [tag state error] :as result} (p state)]
        (case tag
          :consumed-ok (walk [] state result)
          :empty-ok (throw empty-many)
          :consumed-error (walk [] state result)
          :empty-error (empty-ok [] state error))))))

(defn fmap [f p]
  (fn [state]
    (let [{:keys [tag] :as result} (p state)]
      (case tag
        :consumed-ok (update-in result [:value] f)
        :empty-ok (update-in result [:value] f)
        :consumed-error result
        :empty-error result))))

(defn some [p]
  (sequence (fmap (fn [x] (fn [xs] (cons x xs))) p)
            (many p)))

(defn run [p string]
  (let [{:keys [tag value error]} (p (s/->state (CharBuffer/wrap string)))]
    (case tag
      :consumed-ok value
      :empty-ok value
      :consumed-error error
      :empty-error error)))

(defn char [^Character c] (satisfy (fn [c'] (.equals c c'))))

(def letter (satisfy (fn [c] (Character/isLetter c))))

(def digit (satisfy (fn [c] (Character/isDigit c))))

(def whitespace (satisfy (fn [c] (Character/isWhitespace c))))

(def word (some letter))