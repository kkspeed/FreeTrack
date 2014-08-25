(ns gps-atk.lib.coords
  (:import [com.bbn.openmap.proj Ellipsoid Length]
           [com.bbn.openmap.proj.coords LatLonPoint$Double ECEFPoint UTMPoint]))

(defn sqr [x]
  (* x x))

(defn ll->xyz
  "Convert latitude and longitude to x y z"
  [lat long]
  (let [ecef (ECEFPoint/LLtoECEF (LatLonPoint$Double. lat long))]
    [(.getx ecef) (.gety ecef) (.getz ecef)]))

(defn xyz->ll
  "Convert x y z to latitude longitude"
  [^double x ^double y ^double z]
  (let [ll (.getLatLon (ECEFPoint. x y z))]
    [(.getLatitude ll) (.getLongitude ll)]))

(defn xyz-distance [p1 p2]
  (Math/sqrt (reduce #(+ %1 (sqr %2)) 0 (map -  p1 p2))))

(defn lat-long-distance [lat1 long1 lat2 long2]
  (.fromRadians Length/METER (.distance (LatLonPoint$Double. lat1 long1)
                                        (LatLonPoint$Double. lat2 long2))))

(defn ll->utmxy [lat long]
  (let [utm (UTMPoint/LLtoUTM (LatLonPoint$Double. lat long))]
    [(. utm northing) (. utm easting) (. utm zone_number) (. utm zone_letter)]))

(defn utmxy->ll [x y zone zone-letter]
  (let [ll (UTMPoint/UTMtoLL Ellipsoid/WGS_84 x y zone zone-letter nil)]
    [(.getLatitude ll) (.getLongitude ll)]))

(defn random-generator-abs-n [n]
  (fn [] (- n (* (Math/random) (* 2 n)))))

(def random-lat (random-generator-abs-n 90))
(def random-long (random-generator-abs-n 180))

(defn lat-long-in-box? [[la lo] [[ltla ltlo] [rbla rblo]]]
  (and (>= ltla la rbla) (>= rblo lo ltlo)))

(defn generate-lat-long [& ignored-regions]
  (let [pt [(random-lat) (random-long)]]
    (if (some (partial lat-long-in-box? pt) ignored-regions)
      (recur ignored-regions)
      pt)))
