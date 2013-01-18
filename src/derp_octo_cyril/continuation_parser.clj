(ns derp-octo-cyril.continuation-parser
  (:require [clojure.algo.generic.functor :as f])
  (:require [derp-octo-cyril.applicative :as ap])
  (:require [derp-octo-cyril.alternative :as al])
  (:require [derp-octo-cyril.monad :as m])
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.continuation :as c])
  (:require [derp-octo-cyril.error :as e])
  (:require [derp-octo-cyril.state :as s]))

(deftype Parser [p]
  clojure.lang.IFn
  (invoke [this arg]
    (p arg))
  (applyTo [this args]
    (apply p args))
  
  p/AParser
  (label [_ message]
    (Parser.
     (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
       (let [message
             (e/->Expected message)
             add-message
             (fn [a] (update-in a [:error] e/add-message message))]
         (p {:state state
             :consumed-ok consumed-ok
             :empty-ok
             (c/->Continuation
              (fn [{:keys [error] :as a}]
                (if (e/unknown? error)
                  (empty-ok a)
                  (empty-ok (add-message a)))))
             :consumed-error consumed-error
             :empty-error
             (c/->Continuation
              (fn [a] (empty-error (add-message a))))})))))
  (parse [_ input]
    (trampoline p
                {:state (s/->State input (s/->Position 0 0) {})
                 :consumed-ok (c/->Continuation (fn [{:keys [x]}] x))
                 :empty-ok (c/->Continuation (fn [{:keys [x]}] x))
                 :consumed-error (c/->Continuation (fn [{:keys [error]}] error))
                 :empty-error (c/->Continuation (fn [{:keys [error]}] error))}))

  ap/Applicative
  (pure [_ x]
    (Parser.
     (fn [{:keys [state empty-ok]}]
       (empty-ok {:x x
                  :state state
                  :error (e/->unknown (:position state))}))))
  (<*> [_ v]
    (Parser.
     (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
       (let [v (force v)
             apply-merge-continue
             (fn [c f error]
               (c/->Continuation
                (fn [a]
                  (c (-> (update-in a [:x] f)
                         (update-in [:error] e/merge-error error))))))
             apply-continue
             (fn [c f]
               (c/->Continuation
                (fn [a]
                  (c (update-in a [:x] f)))))
             merge-continue
             (fn [c error]
               (c/->Continuation
                (fn [a]
                  (c (update-in a [:error] e/merge-error error)))))]
         (p {:state state
             :consumed-ok
             (c/->Continuation
              (fn [{f :x state :state error :error}]
                (v {:state state
                    :consumed-ok (apply-continue consumed-ok f)
                    :empty-ok (apply-merge-continue consumed-ok f error)
                    :consumed-error consumed-error
                    :empty-error (merge-continue consumed-error error)})))
             :empty-ok
             (c/->Continuation
              (fn [{f :x state :state error :error}]
                (v {:state state
                    :consumed-ok (apply-continue consumed-ok f)
                    :empty-ok (apply-merge-continue empty-ok f error)
                    :consumed-error consumed-error
                    :empty-error (merge-continue empty-error error)})))
             :consumed-error consumed-error
             :empty-error empty-error})))))

  al/Alternative
  (empty [_]
    (Parser.
     (fn [{:keys [state empty-error]}]
       (empty-error {:error (e/->unknown (:position state))}))))
  (combine [_ v]
    (Parser.
     (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
       (let [v (force v)
             merge-continue
             (fn [c error]
               (c/->Continuation
                (fn [a]
                  (c (update-in a [:error] e/merge-error error)))))]
         (p {:state state
             :consumed-ok consumed-ok
             :empty-ok empty-ok
             :consumed-error consumed-error
             :empty-error
             (c/->Continuation
              (fn [{:keys [error]}]
                (v {:state state
                    :consumed-ok consumed-ok
                    :empty-ok (merge-continue empty-ok error)
                    :consumed-error consumed-error
                    :empty-error (merge-continue empty-error error)})))})))))
  (many [_]
    (Parser.
     (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
       (let [t "'many' applied to a parser that accepts an empty input"
             empty-error
             (fn [_]
               (throw (RuntimeException. t)))
             walk
             (fn walk [acc]
               (c/->Continuation
                (fn [{:keys [x state error]}]
                  (let [acc' (conj acc x)]
                    (p {:state state
                        :consumed-ok
                        (c/->Continuation
                         (fn [a] ((walk acc') a)))
                        :empty-ok empty-error
                        :consumed-error consumed-error
                        :empty-error
                        (c/->Continuation
                         (fn [{:keys [error]}]
                           (consumed-ok {:x acc'
                                         :state state
                                         :error error})))})))))]
         (p {:state state
             :consumed-ok (walk [])
             :empty-ok empty-error
             :consumed-error consumed-error
             :empty-error
             (c/->Continuation
              (fn [{:keys [error]}]
                (empty-ok {:x [] :state state :error error})))})))))
  (some [u]
    (ap/<*> (f/fmap (partial partial cons) u) (al/many u)))

  m/Monad
  (bind [_ f]
    (Parser.
     (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
       (let [merge-continue
             (fn [c error]
               (c/->Continuation
                (fn [a]
                  (c (update-in a [:error] e/merge-error error)))))]
         (p {:state state
             :consumed-ok
             (c/->Continuation
              (fn [{:keys [x state error]}]
                (let [consumed-ok-merge (merge-continue consumed-ok error)]
                  ((force (f x)) {:state state
                                  :consumed-ok consumed-ok-merge
                                  :empty-ok consumed-ok-merge
                                  :consumed-error consumed-error
                                  :empty-error
                                  (merge-continue consumed-error error)}))))
             :empty-ok
             (c/->Continuation
              (fn [{:keys [x state error]}]
                ((force (f x)) {:state state
                                :consumed-ok consumed-ok
                                :empty-ok (merge-continue empty-ok error)
                                :consumed-error consumed-error
                                :empty-error
                                (merge-continue empty-error error)})))
             :consumed-error consumed-error
             :empty-error empty-error}))))))

(defmethod f/fmap Parser [f p]
  (Parser.
   (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
     (let [ok-cont
           (fn [c]
             (c/->Continuation
              (fn [a] (c (update-in a [:x] f)))))]
       (p {:state state
           :consumed-ok (ok-cont consumed-ok)
           :empty-ok (ok-cont empty-ok)
           :consumed-error consumed-error
           :empty-error empty-error})))))