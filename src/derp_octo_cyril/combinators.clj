(ns derp-octo-cyril.combinators
  (:refer-clojure :rename {some core-some})
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.error :as e]))

(defn try [p]
  (reify p/Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (p/run p state
             consumed-ok
             empty-ok
             empty-error
             empty-error))))

(def many-error
  (RuntimeException. "many applied to an empty parser"))

(defn many-reduce [f p]
  (reify p/Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (let [walk (fn walk [acc]
                   (fn [x state']
                     (p/run p state'
                            (walk (f acc x))
                            (fn [_ _] (throw many-error))
                            consumed-error
                            (fn [_] #(consumed-ok (f acc x) state')))))]
        (p/run p state
               (walk [])
               (fn [_ _] (throw many-error))
               consumed-error
               (fn [_] #(empty-ok [] state)))))))

(defn many [p]
  (many-reduce conj p))

(defn some [p]
  (p/lift cons p (many p)))

(defn some-separated [p separator]
  (p/lift cons p (many (p/lift (fn [_ x] x) separator p))))

(defn optional [p]
  (p/choose p (p/pure nil)))

(defn not-followed-by [p]
  (try (p/choose (p/bind (try p) (fn [_] p/empty))
                   (p/pure nil))))

(defn look-ahead [p]
  (reify p/Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (p/run p state
             (fn [value _]
               #(consumed-ok value state))
             (fn [value _]
               #(empty-ok value state))
             consumed-error
             empty-error))))

(defn label [p message]
  (reify p/Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (p/run p state
             consumed-ok
             empty-ok
             consumed-error
             (fn [error]
               #(empty-error (e/set-expected error message)))))))

(defn no-label [p]
  (reify p/Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (p/run p state
             consumed-ok
             empty-ok
             consumed-error
             (fn [error]
               #(empty-error (e/remove-expected error)))))))