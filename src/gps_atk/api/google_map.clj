(ns gps-atk.api.google-map
  (:import [java.net URLEncoder])
  (:require [clojure.data.json :as json]
            [clojure.string :as string]
            [ring.util.codec :as codec]
            [gps-atk.lib.coords :as coords]))

(def elevation-base-url "http://maps.google.com/maps/api/elevation/json")

(defn build-path-arg [path]
  (string/join "|" (map (fn [[x y]]
                          (str x "," y)) path)))

(defn make-query-string [m]
  (->> (for [[k v] m]
         (str (codec/url-encode k) "=" (codec/url-encode v)))
       (interpose "&")
       (apply str)))

(defn get-google-elevation-data
  "Get a list of points"
  [{:keys [path samples sensor?] :or {samples 10 sensor? false}}]
  (->> {"path"    (build-path-arg path)
        "sensor"  sensor?
        "samples" samples}
      make-query-string
      (str elevation-base-url "?")
      slurp
      json/read-str))

(defn pair [s]
  (interleave (partition 2 s)
              (partition 2 (rest s))))

(defn location-points [data]
  (map (fn [p]
         [(get-in p ["location" "lat"])
          (get-in p ["location" "lng"])]) (get data "results")))
