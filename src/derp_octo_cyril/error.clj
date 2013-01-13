(ns derp-octo-cyril.error)

(defrecord Expected [message])

(defrecord Unexpected [message])

(defrecord SysUnexpected [message])

(defprotocol AError
  (merge-error [this error])
  (unknown? [this])
  (add-message [this message]))

(defrecord ParseError [messages position]
  AError
  (merge-error [this {:keys [messages']}]
    (->ParseError (concat messages messages') position))
  (unknown? [this]
    (empty? messages))
  (add-message [this message]
    (->ParseError (cons message (remove (partial = message) messages))
                  position)))

(defn ->unknown [position]
  (->ParseError [] position))

(defn ->expected [message position]
  (->ParseError [(->Expected message)] position))

(defn ->unexpected [message position]
  (->ParseError [(->Unexpected message)] position))

(defn ->sys-unexpected [message position]
  (->ParseError [(->SysUnexpected message)] position))