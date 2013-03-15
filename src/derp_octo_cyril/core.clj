(ns derp-octo-cyril.core)

(defn consumed-ok [value state error])
(defn empty-ok [value state error])
(defn consumed-error [state error])
(defn empty-error [state error])

(defn merge-error [error error'])

(defn ^{:private true}
  sequence-c-ok
  [{f :value error :error}
   {tag :tag x :value state :state error' :error :as result}]
  (case tag
    :consumed-ok (consumed-ok (f x) state error')
    :empty-ok (consumed-ok (f x) state (merge-error error error'))
    :consumed-error result
    :empty-error (consumed-error state (merge-error error error'))))

(defn ^{:private true}
  sequence-e-ok
  [{f :value error :error}
   {tag :tag x :value state :state error' :error :as result}]
  (case tag
    :consumed-ok (consumed-ok (f x) state error')
    :empty-ok (empty-ok (f x) state (merge-error error error'))
    :consumed-error result
    :empty-error (empty-error state (merge-error error error'))))

(defn sequence [p q]
  (fn [state]
    (let [{:keys [tag state] :as result} (p state)]
      (case tag
        :consumed-ok (sequence-c-ok result (q state))
        :empty-ok (sequence-e-ok result (q state))
        :consumed-error result
        :empty-error result))))