(ns derp-octo-cyril.parser
  (:require [clojure.algo.generic.functor :as f])
  (:require [derp-octo-cyril.applicative :as ap])
  (:require [derp-octo-cyril.alternative :as al])
  (:require [derp-octo-cyril.monad :as m]))

(defprotocol AParser
  (label [this message])
  (parse [this input]))

(extend-type clojure.lang.Delay
  AParser
  (label [p message]
    (label (force p) message))
  (parse [p input]
    (parse (force p) input))
  
  ap/Applicative
  (pure [a x]
    (ap/pure (force a) x))
  (<*> [u v]
    (ap/<*> (force u) (force v)))
  
  al/Alternative
  (empty [a]
    (al/empty (force a)))
  (combine [u v]
    (al/combine (force u) (force v)))
  (many [u]
    (al/many (force u)))
  (some [u]
    (al/some (force u)))
  
  m/Monad
  (bind [u f]
    (m/bind (force u) f)))

(defmethod f/fmap clojure.lang.Delay [f p]
  (f/fmap f (force p)))

(defn between [pre p post]
  (ap/<* (ap/*> pre p) post))

(defn chainl1 [p op]
  (let [rest
        (fn rest [x]
          (al/combine (m/bind op (fn [f]
                                   (m/bind p (fn [y]
                                               (rest (f x y))))))
                      (ap/pure p x)))]
    (m/bind p rest)))