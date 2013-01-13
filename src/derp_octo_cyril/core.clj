(ns derp-octo-cyril.core
  (:require [derp-octo-cyril.error :as e]))

(defrecord Position [line column])

(defn inc-line [position]
  (update-in position [:line] inc))

(defn inc-column [position]
  (update-in position [:column] inc))

(defrecord State [input position user])

(defn label [p message]
  (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
    (let [message
          (e/->Expected message)
          add-message'
          (fn [a] (update-in a [:error] e/add-message message))]
      (p {:state state
          :consumed-ok consumed-ok
          :empty-ok (fn [{:keys [error] :as a}]
                      (if (e/unknown? error)
                        (empty-ok a)
                        (empty-ok (add-message' a))))
          :consumed-error consumed-error
          :empty-error (fn [a]
                         (empty-error (add-message' a)))}))))

(defn token-primitive [{:keys [show update-position update-state predicate]}]
  (fn [{:keys [state consumed-ok empty-error]}]
    (let [{:keys [input position user]} state
          [token & r] input]
      (cond (empty? input)
            (empty-error {:error (e/->sys-unexpected "end of input" position)})
            (predicate token)
            (let [new-position (update-position {:position position
                                                 :token token
                                                 :tokens r})
                  new-user-state (update-state {:position position
                                                :token token
                                                :tokens r
                                                :state user})
                  new-state (->State r new-position new-user-state)]
              (consumed-ok {:token token
                            :state new-state
                            :error (e/->unknown new-position)}))
            :else
            (empty-error {:error (e/->sys-unexpected (show token) position)})))))

(def letter
  (token-primitive {:show (fn [c] (str "'" c "'"))
                    :update-position (fn [{:keys [position token]}]
                                       (if (= token \newline)
                                         (inc-line position)
                                         (inc-column position)))
                    :update-state identity
                    :predicate (fn [c] (Character/isLetter c))}))

(defn p-char [c]
  (label (token-primitive {:show (fn [c] (str "'" c "'"))
                           :update-position (fn [{:keys [position token]}]
                                              (if (= token \newline)
                                                (inc-line position)
                                                (inc-column position)))
                           :update-state identity
                           :predicate (partial = c)})
         (str "'" c "'")))

(defn p-symbol [s]
  (token-primitive {:show str
                    :update-position (fn [{:keys [position]}]
                                       (inc-column position))
                    :update-state identity
                    :predicate (partial = s)}))

(defn parse [parser input]
  (parser {:state (->State input (->Position 0 0) {})
           :consumed-ok (fn [{:keys [token]}] token)
           :empty-ok (constantly nil)
           :consumed-error (fn [{:keys [error]}] error)
           :empty-error (fn [{:keys [error]}] error)}))

(defn fmap [f p]
  (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
    (let [ok-cont (fn [c] (fn [a] (c (update-in a [:token] f))))]
      (p {:state state
          :consumed-ok (ok-cont consumed-ok)
          :empty-ok (ok-cont empty-ok)
          :consumed-error consumed-error
          :empty-error empty-error}))))

(defn pure [x]
  (fn [{:keys [state empty-ok]}]
    (empty-ok {:token x :state state :error (e/->unknown (:position state))})))

(defn <*>
  ([af ax]
     (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
       (let [apply-f
             (fn [f]
               (fn [a] (update-in a [:token] f)))
             merge-errors
             (fn [error]
               (fn [a] (update-in a [:error] e/merge-error error)))]
         (af {:state state
              :consumed-ok
              (fn [{f :token state :state error :error}]
                (ax {:state state
                     :consumed-ok (comp consumed-ok
                                        (apply-f f))
                     :empty-ok (comp consumed-ok
                                     (merge-errors error)
                                     (apply-f f))
                     :consumed-error consumed-error
                     :empty-error (comp consumed-error
                                        (merge-errors error))}))
              :empty-ok
              (fn [{f :token state :state error :error}]
                (ax {:state state
                     :consumed-ok (comp consumed-ok
                                        (apply-f f))
                     :empty-ok (comp empty-ok
                                     (merge-errors error)
                                     (apply-f f))
                     :consumed-error consumed-error
                     :empty-error (comp empty-error
                                        (merge-errors error))}))
              :consumed-error consumed-error
              :empty-error empty-error}))))
  ([af ax & r]
     (reduce <*> (<*> af ax) r)))

(defn *>
  ([aa ab] (<*> (pure (constantly identity)) aa ab))
  ([aa ab & r] (reduce *> (*> aa ab) r)))

(def p-empty
  (fn [{:keys [state empty-error]}]
    (empty-error {:error (e/->unknown (:position state))})))

(defn p-or
  ([pa pb]
     (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
       (let [merge-errors
             (fn [error]
               (fn [a] (update-in a [:error] e/merge-error error)))]
         (pa {:state state
              :consumed-ok consumed-ok
              :empty-ok empty-ok
              :consumed-error consumed-error
              :empty-error (fn [{:keys [error]}]
                             (pb {:state state
                                  :consumed-ok consumed-ok
                                  :empty-ok (comp empty-ok
                                                  (merge-errors error))
                                  :consumed-error consumed-error
                                  :empty-error (comp empty-error
                                                     (merge-errors error))}))}))))
  ([pa pb & r] (reduce p-or (p-or pa pb) r)))

(defn p-many [p]
  (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
    (let [empty-error
          (fn [_]
            (throw(RuntimeException. "'many' applied to a parser that accepts an empty input")))
          walk
          (fn walk [acc]
            (fn [{:keys [token state error]}]
              (let [acc' (conj acc token)]
                (p {:state state
                    :consumed-ok (walk acc')
                    :empty-ok empty-error
                    :consumed-error consumed-error
                    :empty-error (fn [{:keys [error]}]
                                   (consumed-ok {:token acc'
                                                 :state state
                                                 :error error}))}))))]
      (p {:state state
          :consumed-ok (walk [])
          :empty-ok empty-error
          :consumed-error consumed-error
          :empty-error (fn [{:keys [error]}]
                         (empty-ok {:token [] :state state :error error}))}))))

(defn p-some [p]
  (<*> (fmap (partial partial cons) p) (p-many p)))