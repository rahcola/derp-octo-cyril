(ns derp-octo-cyril.core
  (:gen-class)
  (:require [derp-octo-cyril.parser :as p])
  (:require [derp-octo-cyril.character :as c])
  (:require [derp-octo-cyril.sequence :as seq])
  (:require [derp-octo-cyril.applicative :as ap])
  (:require [derp-octo-cyril.alternative :as al])
  (:require [derp-octo-cyril.monad :as m])
  (:require [derp-octo-cyril.error :as e])
  (:require [derp-octo-cyril.state :as s])
  (:require [derp-octo-cyril.bytestring :as b]))

(declare expr term fact)

(def expr
  (delay
   (p/chainl1 term (al/combine (ap/<$ + (c/character \+))
                               (ap/<$ - (c/character \-))))))

(def term
  (delay
   (p/chainl1 fact (al/combine (ap/<$ * (c/character \*))
                               (ap/<$ / (c/character \/))))))

(def fact
  (delay
   (al/combine c/decimal
               (p/between (c/character \() expr (c/character \))))))

(defn gen-expr [n]
  (if (zero? n)
    "1"
    (let [expr' (gen-expr (dec n))]
      (str expr' "+" expr' "*(" expr' "-" expr' ")"))))

(defn -main [& args]
  (let [expr-level (read-string (first args))
        expr-str (gen-expr expr-level)
        times' (read-string (second args))]
    (loop [acc []
           times times']
      (if (zero? times)
        (println (int (/ (/ (apply + acc) times') 1000000)))
        (let [input (b/->ByteString expr-str)
              start (System/nanoTime)]
          (p/parse expr input)
          (recur (conj acc (- (System/nanoTime) start))
                 (dec times)))))))