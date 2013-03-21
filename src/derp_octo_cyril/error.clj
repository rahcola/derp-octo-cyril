(ns derp-octo-cyril.error
  (:refer-clojure :rename {concat core-concat}))

(defrecord Expected [message]
  Object
  (toString [_]
    (str "expected " message)))

(defrecord Unexpected [message]
  Object
  (toString [_]
    (str "unexpected " message)))

(defrecord SysUnexpected [message]
  Object
  (toString [_]
    (str "unexpected " message)))

(defprotocol ParseError
  (concat [self error])
  (add-expect [self message]))

(defrecord AParseError [messages position]
  ParseError
  (concat [error {:keys [messages' position'] :as error'}]
    (cond (empty? messages) error'
          (empty? messages') error
          :else
          (case (compare position position')
            0 (AParseError. (core-concat messages messages') position)
            -1 error'
            1 error)))
  (add-expect [self message]
    (if (empty? messages)
      self
      (AParseError. (cons (->Expected message)
                          (remove (partial = message) messages))
                    position)))
  Object
  (toString [_]
    (str position ":"
         (apply str (interpose \newline (map str messages))))))

(defmethod clojure.core/print-method AParseError
  [error writer]
  (.write writer (str error)))

(defn unknown [position]
  (->AParseError [] position))

(defn expected [message position]
  (->AParseError [(->Expected message)] position))

(defn unexpected [message position]
  (->AParseError [(->Unexpected message)] position))

(defn sys-unexpected [message position]
  (->AParseError [(->SysUnexpected message)] position))