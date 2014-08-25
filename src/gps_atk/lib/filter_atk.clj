(ns gps-atk.lib.filter-atk
  (:require [gps-atk.lib.coords :as coords]))

(defn get-test-point [center dim radius]
  (let [[x y zone zone-letter] (apply coords/ll->utmxy center)]
    (if (= dim :x)
      (coords/utmxy->ll (- x radius) y zone zone-letter)
      (coords/utmxy->ll x (- y radius) zone zone-letter))))

(defn- update-by-test
  "See if the point is inside, FIXME: we have to make
   asymetric shift as compensate for the uncovered space"
  [in? val radius possible]
  (or (and in? (- val (quot radius possible)))
      (+ val (* 0.66 (quot radius possible)))))

(defn recenter [center dim radius in? possible]
  (let [[x y zone zone-letter] (apply coords/ll->utmxy center)]
    [(if (= dim :x)
       (coords/utmxy->ll (update-by-test in? x radius possible)
                         y zone zone-letter)
       (coords/utmxy->ll x (update-by-test in? y radius possible)
                         zone zone-letter))
     (first (remove #{dim} [:x :y]))]))
