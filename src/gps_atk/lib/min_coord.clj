(ns gps-atk.lib.min-coord
  (:require [gps-atk.lib.octave :as octave]
            [gps-atk.lib.coords :as coords]
            [clojure.string :as string]))

(def octave-shell octave/octave-eval)

(def target-function
  ["function out=target_fn(X)"
   "points ="
   "distances ="
   "sum = 0;"
   "[col, N] = size(distances);"
   "for i=1 : N"
   "v=points(:, i);"
   "sum = sum + ((transpose (v-X)) * (v-X)-distances(i)^2)^2;"
   "endfor"
   "out = sum;"
   "endfunction"])

(defn make-coords-matrix [coords]
  (str "[" (string/join ";" (map str coords)) "]"))

(defn update-octave-function [coords distances]
  (string/join "\n"
               (assoc target-function
                 1 (str "points = transpose(" (make-coords-matrix coords) ");")
                 2 (str "distances = " (str distances) ";"))))

(defn least-square-position
  "Find the least square of the coordinates"
  [cols]
  (octave/with-octave o
    (octave-shell o "pkg load optim")
    (let [coords (mapv (comp (partial apply coords/ll->xyz) :coord) cols)
          distances (mapv :dist cols)]
      (octave-shell o (update-octave-function coords distances))
      (apply
       coords/xyz->ll
       (octave-shell o "bfgsmin('target_fn', {[1;1;1]}, {100});"
                     :var "ans")))))
