(ns derp-octo-cyril.core
  (:require [derp-octo-cyril.continuation :as c])
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
           (fn [a] (empty-error (add-message a))))}))))

(defn token-primitive [{:keys [show update-position update-state predicate]}]
  (fn [{:keys [state consumed-ok empty-error]}]
    (let [{:keys [input position user]} state]
      (let [[token & r] input]
        (cond (empty? input)
              (empty-error {:error (e/->sys-unexpected "" position)})
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
              (empty-error
               {:error (e/->sys-unexpected (show token) position)}))))))

(defn p-take-while [predicate]
  (fn [{:keys [state consumed-ok empty-ok]}]
    (let [{:keys [input position user]} state
          continue
          (fn [acc input position user]
            ((if (empty? acc) empty-ok consumed-ok)
             {:token acc
              :state (->State input position user)
              :error (e/->unknown position)}))]
      (loop [[token & r :as input] input
             position position
             acc []]
        (cond (empty? input)
              (continue acc input position user)
              (predicate token)
              (recur r
                     (inc-column position)
                     (conj acc token))
              :else
              (continue acc input position user))))))

(defn parse [parser input]
  (trampoline parser
              {:state (->State input (->Position 0 0) {})
               :consumed-ok (c/->Continuation (fn [{:keys [token]}] token))
               :empty-ok (c/->Continuation (fn [{:keys [token]}] token))
               :consumed-error (c/->Continuation (fn [{:keys [error]}] error))
               :empty-error (c/->Continuation (fn [{:keys [error]}] error))}))

(defn fmap [f p]
  (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
    (let [ok-cont
          (fn [c]
            (c/->Continuation
             (fn [a] (c (update-in a [:token] f)))))]
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
       (let [apply-merge-continue
             (fn [c f error]
               (c/->Continuation
                (fn [a]
                  (c (-> (update-in a [:token] f)
                         (update-in [:error] e/merge-error error))))))
             apply-continue
             (fn [c f]
               (c/->Continuation
                (fn [a]
                  (c (update-in a [:token] f)))))
             merge-continue
             (fn [c error]
               (c/->Continuation
                (fn [a]
                  (c (update-in a [:error] e/merge-error error)))))]
         (af {:state state
              :consumed-ok
              (c/->Continuation
               (fn [{f :token state :state error :error}]
                 (ax {:state state
                      :consumed-ok (apply-continue consumed-ok f)
                      :empty-ok (apply-merge-continue consumed-ok f error)
                      :consumed-error consumed-error
                      :empty-error (merge-continue consumed-error error)})))
              :empty-ok
              (c/->Continuation
               (fn [{f :token state :state error :error}]
                 (ax {:state state
                      :consumed-ok (apply-continue consumed-ok f)
                      :empty-ok (apply-merge-continue empty-ok f error)
                      :consumed-error consumed-error
                      :empty-error (merge-continue empty-error error)})))
              :consumed-error consumed-error
              :empty-error empty-error}))))
  ([af ax & r]
     (reduce <*> (<*> af ax) r)))

(defn *>
  ([aa ab] (<*> (pure (constantly identity)) aa ab))
  ([aa ab & r] (reduce *> (*> aa ab) r)))

(defn <*
  ([aa ab] (<*> (pure constantly) aa ab))
  ([aa ab & r] (reduce <* (<* aa ab) r)))

(def p-empty
  (fn [{:keys [state empty-error]}]
    (empty-error {:error (e/->unknown (:position state))})))

(defn p-or
  ([pa pb]
     (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
       (let [merge-continue
             (fn [c error]
               (c/->Continuation
                (fn [a]
                  (c (update-in a [:error] e/merge-error error)))))]
         (pa {:state state
              :consumed-ok consumed-ok
              :empty-ok empty-ok
              :consumed-error consumed-error
              :empty-error
              (c/->Continuation
               (fn [{:keys [error]}]
                 (pb {:state state
                      :consumed-ok consumed-ok
                      :empty-ok (merge-continue empty-ok error)
                      :consumed-error consumed-error
                      :empty-error (merge-continue empty-error error)})))}))))
  ([pa pb & r] (reduce p-or (p-or pa pb) r)))

(defn p-many [p]
  (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
    (let [t "'many' applied to a parser that accepts an empty input"
          empty-error
          (fn [_]
            (throw (RuntimeException. t)))
          walk
          (fn walk [acc]
            (c/->Continuation
             (fn [{:keys [token state error]}]
               (let [acc' (conj acc token)]
                 (p {:state state
                     :consumed-ok
                     (c/->Continuation
                      (fn [a] ((walk acc') a)))
                     :empty-ok empty-error
                     :consumed-error consumed-error
                     :empty-error
                     (c/->Continuation
                      (fn [{:keys [error]}]
                        (consumed-ok {:token acc'
                                      :state state
                                      :error error})))})))))]
      (p {:state state
          :consumed-ok (walk [])
          :empty-ok empty-error
          :consumed-error consumed-error
          :empty-error
          (c/->Continuation
           (fn [{:keys [error]}]
             (empty-ok {:token [] :state state :error error})))}))))

(defn p-some [p]
  (<*> (fmap (partial partial cons) p) (p-many p)))

(defn bind [p f]
  (fn [{:keys [state consumed-ok empty-ok consumed-error empty-error]}]
    (let [merge-continue
          (fn [c error]
            (c/->Continuation
             (fn [a]
               (c (update-in a [:error] e/merge-error error)))))]
      (p {:state state
          :consumed-ok
          (c/->Continuation
           (fn [{:keys [token state error]}]
             (let [consumed-ok-merge (merge-continue consumed-ok error)]
               ((f token) {:state state
                           :consumed-ok  consumed-ok-merge
                           :empty-ok  consumed-ok-merge
                           :consumed-error consumed-error
                           :empty-error (merge-continue consumed-error error)}))))
          :empty-ok
          (c/->Continuation
           (fn [{:keys [token state error]}]
             ((f token) {:state state
                         :consumed-ok consumed-ok
                         :empty-ok (merge-continue empty-ok error)
                         :consumed-error consumed-error
                         :empty-error (merge-continue empty-error error)})))
          :consumed-error consumed-error
          :empty-error empty-error}))))

;; test

(defn chainl1 [p op]
  (let [rest
        (fn rest [x]
          (p-or (bind op (fn [f]
                           (bind p (fn [y]
                                     (c/->Continuation
                                      (fn [a]
                                        ((rest (f x y)) a)))))))
                (pure x)))]
    (bind p rest)))

(def letter
  (label (token-primitive {:show pr-str
                           :update-position
                           (fn [{:keys [position token]}]
                             (if (= token \newline)
                               (inc-line position)
                               (inc-column position)))
                           :update-state identity
                           :predicate
                           (fn [^Character c] (Character/isLetter c))})
         "letter"))

(def digit
  (label (token-primitive {:show pr-str
                           :update-position (fn [{:keys [position token]}]
                                              (if (= token \newline)
                                                (inc-line position)
                                                (inc-column position)))
                           :update-state identity
                           :predicate (fn [^Character c] (Character/isDigit c))})
         "digit"))

(def decimal
  (label (bind (p-take-while (fn [^Character c] (Character/isDigit c)))
               (fn [s]
                 (if (empty? s)
                   p-empty
                   (pure (Integer/parseInt (reduce str "" s))))))
         "decimal"))

(defn p-char [^Character c]
  (label (token-primitive {:show pr-str
                           :update-position (fn [{:keys [position token]}]
                                              (if (= token \newline)
                                                (inc-line position)
                                                (inc-column position)))
                           :update-state identity
                           :predicate (fn [^Character c']
                                        (.equals c c'))})
         (str "'" c "'")))

(defn p-symbol [s]
  (label (token-primitive {:show pr-str
                           :update-position (fn [{:keys [position]}]
                                              (inc-column position))
                           :update-state identity
                           :predicate (partial = s)})
         (str s)))

(declare term)
(def expr
  (chainl1 #'term (p-or (<* (pure +) (p-char \+))
                        (<* (pure -) (p-char \-)))))

(declare fact)
(def term
  (chainl1 #'fact (p-or (<* (pure *) (p-char \*))
                        (<* (pure /) (p-char \/)))))

(def fact
  (p-or decimal
        (<* (*> (p-char \()
                #'expr)
            (p-char \)))))

(defn gen-expr [n]
  (if (zero? n)
    "1"
    (let [expr' (gen-expr (dec n))]
      (str expr' "+" expr' "*(" expr' "-" expr' ")"))))

(defn -main [& args]
  (let [input (gen-expr (read-string (first args)))
        times' (read-string (second args))]
    (loop [acc []
           times times']
      (if (zero? times)
        (println (int (/ (/ (apply + acc) times') 1000000)))
        (let [start (System/nanoTime)]
          (parse expr input)
          (recur (conj acc (- (System/nanoTime) start))
                 (dec times)))))))