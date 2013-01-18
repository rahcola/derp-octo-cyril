(ns derp-octo-cyril.bytestring
  (:refer-clojure :exclude [empty? first rest split-with count]))

(defprotocol AByteString
  (empty? [this])
  (first [this])
  (rest [this])
  (split-with [this pred])
  (count [this]))

(deftype ByteString [^chars arr start end]
  AByteString
  (empty? [_]
    (> start end))
  (first [this]
    (if (empty? this)
      nil
      (aget arr start)))
  (rest [this]
    (if (empty? this)
      this
      (ByteString. arr (inc start) end)))
  (split-with [this pred]
    (loop [new-start start]
      (cond (> new-start end)
            [this (ByteString. (char-array 0) 1 0)]
            (pred (aget arr new-start))
            (recur (inc new-start))
            :else
            [(ByteString. arr start (dec new-start))
             (ByteString. arr new-start end)])))
  (count [_]
    (inc (- end start)))

  Object
  (toString [_]
    (String. arr (int start) (int (inc (- end start))))))

(defn ->ByteString [^String s]
  (ByteString. (char-array s) 0 (dec (.length s))))