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
  (unknown? [self])
  (add-message [self message]))

(defrecord AParseError [messages position]
  ParseError
  (concat [_ {:keys [messages']}]
    (->AParseError (core-concat messages messages') position))
  (unknown? [_]
    (empty? messages))
  (add-message [_ message]
    (->AParseError (cons message (remove (partial = message) messages))
                   position))
  Object
  (toString [_]
    (if (empty? messages)
      "unknown parse error"
      (str position ": " (apply str (interpose \newline (map str messages)))))))

(defmethod clojure.core/print-method AParseError
  [error writer]
  (.write writer (str error)))

(defn ->unknown [position]
  (->AParseError [] position))

(defn ->expected [message position]
  (->AParseError [(->Expected message)] position))

(defn ->unexpected [message position]
  (->AParseError [(->Unexpected message)] position))

(defn ->sys-unexpected [message position]
  (->AParseError [(->SysUnexpected message)] position))