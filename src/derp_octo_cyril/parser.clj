(ns derp-octo-cyril.parser
  (:refer-clojure :rename {empty core-empty
                           some core-some
                           sequence core-sequence})
  (:require [derp-octo-cyril.state :as s])
  (:require [derp-octo-cyril.error :as e]))

(defprotocol Parser
  (run [p state consumed-ok empty-ok consumed-error empty-error]))

(defprotocol ParserInput
  (parse [input parser]))

(extend-type clojure.lang.Delay
  Parser
  (run [d state consumed-ok empty-ok consumed-error empty-error]
    (run (force d) state consumed-ok empty-ok consumed-error empty-error)))

(defn pure [x]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (empty-ok x state))))

(defn sequence
  ([p q]
     (reify Parser
       (run [_ state consumed-ok empty-ok consumed-error empty-error]
         (run p state
              (fn [f state']
                (let [cok (fn [x state]
                            (consumed-ok (f x) state))]
                  (run q state'
                       cok
                       cok
                       consumed-error
                       consumed-error)))
              (fn [f state']
                (run q state'
                     (fn [x state'']
                       (consumed-ok (f x) state''))
                     (fn [x state'']
                       (empty-ok (f x) state''))
                     consumed-error
                     empty-error))
              consumed-error
              empty-error))))
  ([p q & rest]
     (reduce sequence (sequence p q) rest)))

(defn bind
  [p f]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (run p state
           (fn [x state']
             (run (f x) state'
                  consumed-ok
                  consumed-ok
                  consumed-error
                  consumed-error))
           (fn [x state']
             (run (f x) state'
                  consumed-ok
                  empty-ok
                  consumed-error
                  empty-error))
           consumed-error
           empty-error))))

(defn try [p]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (run p state
           consumed-ok
           empty-ok
           empty-error
           empty-error))))

(defn ^{:private true}
  curry [n f]
  (if (< n 2)
    f
    (fn [a]
      (curry (dec n) (partial f a)))))

(defn lift
  [f a & args]
  (let [n (inc (count args))]
    (apply sequence (pure (curry n f)) a args)))

(def empty
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (empty-error (e/unknown (:position state))))))

(defn choose
  ([p q]
     (reify Parser
       (run [_ state consumed-ok empty-ok consumed-error empty-error]
         (run p state
              consumed-ok
              empty-ok
              consumed-error
              (fn [error]
                (run q state
                     consumed-ok
                     empty-ok
                     consumed-error
                     (fn [error']
                       (empty-error (e/merge error error')))))))))
  ([p q & rest]
     (reduce choose (choose p q) rest)))

(def many-error
  (RuntimeException. "many applied to an empty parser"))

(defn many-reduce [f p]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (let [walk (fn walk [acc]
                   (fn [x state']
                     (run p state'
                          (walk (f acc x))
                          (fn [_ _] (throw many-error))
                          consumed-error
                          (fn [_] (consumed-ok (f acc x) state')))))]
        (run p state
             (walk [])
             (fn [_ _] (throw many-error))
             consumed-error
             (fn [_] (empty-ok [] state)))))))

(defn many [p]
  (many-reduce conj p))

(defn some [p]
  (lift cons p (many p)))

(defn some-separated [p separator]
  (lift cons p (many (lift (fn [_ x] x) separator p))))

(defn optional [p]
  (choose p (pure nil)))

(defn not-followed-by [p]
  (try (choose (bind (try p) (fn [_] empty))
               (pure nil))))

(defn look-ahead [p]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (run p state
           (fn [value _]
             (consumed-ok value state))
           (fn [value _]
             (empty-ok value state))
           consumed-error
           empty-error))))

(defn label [p message]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (run p state
           consumed-ok
           empty-ok
           consumed-error
           (fn [error]
             (empty-error (e/set-expected error message)))))))

(defn no-label [p]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (run p state
           consumed-ok
           empty-ok
           consumed-error
           (fn [error]
             (empty-error (e/remove-expected error)))))))

(def source-position
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (empty-ok (:position state) state))))