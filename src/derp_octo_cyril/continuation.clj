(ns derp-octo-cyril.continuation)

(deftype Continuation [f]
  clojure.lang.IFn
  (invoke [this arg]
    #(f arg))
  (applyTo [this args]
    #(apply this args)))