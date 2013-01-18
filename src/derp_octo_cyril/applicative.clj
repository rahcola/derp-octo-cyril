(ns derp-octo-cyril.applicative
  (:require [clojure.algo.generic.functor :as f]))

(defprotocol Applicative
  (pure [_ x])
  (<*> [u v]))

(defn *> [u v]
  (<*> (<*> (pure u (constantly identity)) u) v))

(defn <* [u v]
  (<*> (<*> (pure u constantly) u) v))

(defn <$ [a u]
  (f/fmap (constantly a) u))

(defn liftA [f a]
  (<*> (pure a f) a))

(defn liftA2 [f a b]
  (<*> (f/fmap (fn [x] (fn [y] (f x y))) a) b))

(defn liftA3 [f a b c]
  (<*> (<*> (f/fmap (fn [x] (fn [y] (fn [z] (f x y z)))) a) b) c))

(defn <**> [v u]
  (liftA2 (fn [x f] (f x)) v u))