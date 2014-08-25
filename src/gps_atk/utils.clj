(ns gps-atk.utils)

(defn timestamp []
  (quot (System/currentTimeMillis) 1000))

(defn seq1
  ;; A sequence processing function to avoid chunking of
  ;; clojure sequences
  [#^clojure.lang.ISeq s]
  (reify clojure.lang.ISeq
    (first [_] (.first s))
    (more [_] (seq1 (.more s)))
    (next [_] (let [sn (.next s)] (and sn (seq1 sn))))
    (seq [_] (let [ss (.seq s)] (and ss (seq1 ss))))
    (count [_] (.count s))
    (cons [_ o] (.cons s o))
    (empty [_] (.empty s))
    (equiv [_ o] (.equiv s o))))
