(ns derp-octo-cyril.polymorphism-parser
  (:require [clojure.algo.generic.functor :as f])
  (:require [derp-octo-cyril.applicative :as ap])
  (:require [derp-octo-cyril.alternative :as al])
  (:require [derp-octo-cyril.monad :as m])
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.continuation :as c])
  (:require [derp-octo-cyril.error :as e])
  (:require [derp-octo-cyril.state :as s]))

(defprotocol AResponse
  (label [this message])
  (parse [this])
  (then [this v])
  (after-consumed-ok [this f error])
  (after-empty-ok [this f error])
  (combine [this v state])
  (fall-back [this error])
  (bind [this f])
  (bind-after-consumed-ok [this error])
  (bind-after-empty-ok [this error]))

(defrecord ConsumedOk [x state error]
  AResponse
  (label [this _]
    this)
  (parse [_] x)
  
  (then [_ u]
    (after-consumed-ok (u state) x error))
  (after-consumed-ok [_ f _]
    (ConsumedOk. (f x) state error))
  (after-empty-ok [_ f _]
    (ConsumedOk. (f x) state error))

  (combine [this _ _]
    this)
  (fall-back [this _]
    this)

  (bind [_ f]
    (bind-after-consumed-ok ((force (f x)) state) error))
  (bind-after-consumed-ok [_ error']
    (ConsumedOk. x state (e/merge-error error' error)))
  (bind-after-empty-ok [this _]
    this))

(defrecord EmptyOk [x state error]
  AResponse
  (label [this message]
    (if (e/unknown? error)
      this
      (EmptyOk. x state (e/add-message error message))))
  (parse [_] x)
  
  (then [_ u]
    (after-empty-ok (u state) x error))
  (after-consumed-ok [_ f error']
    (ConsumedOk. (f x) state (e/merge-error error' error)))
  (after-empty-ok [_ f error']
    (EmptyOk. (f x) state (e/merge-error error' error)))

  (combine [this _ _]
    this)
  (fall-back [_ error']
    (EmptyOk. x state (e/merge-error error' error)))

  (bind [_ f]
    (bind-after-empty-ok ((force (f x)) state) error))
  (bind-after-consumed-ok [_ error']
    (ConsumedOk. x state (e/merge-error error' error)))
  (bind-after-empty-ok [_ error']
    (EmptyOk. x state (e/merge-error error' error))))

(defrecord ConsumedError [error]
  AResponse
  (label [this _]
    this)
  (parse [_] error)

  (then [this _]
    this)
  (after-consumed-ok [this _ _]
    this)
  (after-empty-ok [this _ _]
    this)

  (combine [this _ _]
    this)
  (fall-back [this _]
    this)

  (bind [this _]
    this)
  (bind-after-consumed-ok [this _]
    this)
  (bind-after-empty-ok [this _]
    this))

(defrecord EmptyError [error]
  AResponse
  (label [this message]
    (EmptyError. (e/add-message error message)))
  (parse [_] error)

  (then [this _]
    this)
  (after-consumed-ok [_ _ error']
    (ConsumedError. (e/merge-error error' error)))
  (after-empty-ok [_ _ error']
    (EmptyError. (e/merge-error error' error)))

  (combine [_ u state]
    (fall-back (u state) error))
  (fall-back [_ error']
    (EmptyError. (e/merge-error error' error)))

  (bind [this _]
    this)
  (bind-after-consumed-ok [_ error']
    (ConsumedError. (e/merge-error error' error)))
  (bind-after-empty-ok [_ error']
    (EmptyError. (e/merge-error error' error))))

(deftype Parser [p]
  clojure.lang.IFn
  (invoke [_ state]
    (p {:state state
        :consumed-ok map->ConsumedOk
        :empty-ok map->EmptyOk
        :consumed-error map->ConsumedError
        :empty-error map->EmptyError}))
  (applyTo [this args]
    (apply this args))
  
  p/AParser
  (label [_ message]
    (Parser. (fn [a] (label (p a) (e/->Expected message)))))
  (parse [this input]
    (parse (this (s/->State input (s/->Position 0 0) {}))))

  ap/Applicative
  (pure [_ x]
    (Parser.
     (fn [{:keys [state empty-ok]}]
       (empty-ok {:x x :state state :error (e/->unknown (:position state))}))))
  (<*> [_ v]
    (Parser. (fn [a] (then (p a) (force v)))))

  al/Alternative
  (empty [_]
    (Parser.
     (fn [{:keys [state empty-error]}]
       (empty-error {:error (e/->unknown (:position state))}))))
  (combine [_ v]
    (Parser. (fn [a] (combine (p a) (force v) (:state a)))))
  (many [_]
    (throw (RuntimeException. "many not implemented")))
  (some [u]
    (ap/<*> (f/fmap (partial partial cons) u) (al/many u)))

  m/Monad
  (bind [_ f]
    (Parser.
     (fn [a]
       (bind (p a) f)))))

(defmethod f/fmap ConsumedOk [f r]
  (update-in r [:x] f))

(defmethod f/fmap EmptyOk [f r]
  (update-in r [:x] f))

(defmethod f/fmap ConsumedError [f r]
  r)

(defmethod f/fmap EmptyError [f r]
  r)

(defmethod f/fmap Parser [f p]
  (Parser.
   (fn [{:keys [state]}]
     (f/fmap f (p state)))))