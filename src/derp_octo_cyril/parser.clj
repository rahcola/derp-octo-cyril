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
         (let [after-ok
               (fn [after-empty-ok after-empty-error]
                 (fn [f state error]
                   (run q state
                        ;; consumed-ok
                        (fn [x state']
                          (consumed-ok (f x) state'))
                        ;; empty-ok
                        (fn [x state']
                          (after-empty-ok (f x) state'))
                        ;; consumed-error
                        consumed-error
                        ;; empty-error
                        (fn [error']
                          (after-empty-error (e/merge error error'))))))]
           (run p state
                (after-ok consumed-ok consumed-error)
                (after-ok empty-ok empty-error)
                consumed-error
                empty-error)))))
  ([p q & rest]
     (reduce sequence (sequence p q) rest)))

(defn bind
  [p f]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (let [after-ok
            (fn [after-empty-ok after-empty-error]
              (fn [x state error]
                (run (f x) state
                     ;; consumed-ok
                     (fn [x' state']
                       (consumed-ok x' state'))
                     ;; empty-ok
                     (fn [x' state']
                       (after-empty-ok x' state'))
                     ;; consumed-error
                     consumed-error
                     ;; empty-error
                     (fn [error']
                       (after-empty-error (e/merge error error'))))))]
        (run p state
             (after-ok consumed-ok consumed-error)
             (after-ok empty-ok empty-error)
             consumed-error
             empty-error)))))

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
                     (fn [value state']
                       (empty-ok value state'))
                     consumed-error
                     (fn [error']
                       (empty-error (e/merge error error')))))))))
  ([p q & rest]
     (reduce choose (choose p q) rest)))

(defn many [p]
  (reify Parser
    (run [_ state consumed-ok empty-ok consumed-error empty-error]
      (let [empty-many-error (RuntimeException. "many applied to an empty parser")
            walk (fn walk [acc]
                   (fn [value state]
                     (run p state
                          (walk (conj acc value))
                          (fn [_ _] (throw empty-many-error))
                          consumed-error
                          (fn [error]
                            (consumed-ok (conj acc value) state)))))]
        (run p state
             (walk [])
             (fn [_ _] (throw empty-many-error))
             consumed-error
             (fn [error] (empty-ok [] state)))))))

(defn some [p]
  (lift cons p (many p)))

(defn some-separated [p separator]
  (lift cons p (many (lift (fn [_ x] x) separator p))))

(defn optional [p]
  (choose p (pure nil)))

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