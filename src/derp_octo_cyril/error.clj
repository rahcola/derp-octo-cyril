(ns derp-octo-cyril.error
  (:refer-clojure :rename {merge core-merge}))

(defrecord Unexpected [message position]
  Object
  (toString [_]
    (str message " at " position)))

(defprotocol ParseError
  (merge [error error'])
  (set-expected [error message])
  (remove-expected [error]))

(defrecord AParseError [expected unexpected]
  ParseError
  (merge [_ {expected' :expected unexpected' :unexpected}]
    (AParseError. (concat expected expected')
                  (concat unexpected unexpected')))
  (set-expected [self message]
    (AParseError. [message] unexpected))
  (remove-expected [self]
    (AParseError. [] unexpected))
  Object
  (toString [_]
    (str "unexpected " (apply str (interpose ", " unexpected))
         \newline
         "expected " (apply str (interpose ", " expected)))))

(defmethod print-method AParseError
  [error writer]
  (.write writer (str error)))

(defn unknown [position]
  (->AParseError [] [(->Unexpected "" position)]))

(defn expected [message]
  (->AParseError [message] []))

(defn unexpected [message position]
  (->AParseError [] [(->Unexpected message position)]))