(ns gps-atk.synthetic
  (:import [java.net Socket InetSocketAddress])
  (:require [gps-atk.schedule :as schedule]
            [gps-atk.core :as core]
            [gps_atk.telnet :as telnet]
            [gps-atk.lib.coords :as coords]
            [gps-atk.lib.filter-atk :as fatk]
            [gps-atk.device :as device]
            [gps-atk.api.google-map :as google]
            [clojure.java.shell :as sh]
            [clj-http.client :as http]))

(defn make-attack-test [{:keys [target port output fn-refresh user conf]}]
  (fn [s]
    ;; let [test-socket (doto (Socket.)
    ;;                    (.connect (InetSocketAddress. target port) 3000)
    ;;                    (telnet/telnet-read-line)
    ;;                    (telnet/telnet-send-line "geo gpsSpoofer start"))]
    (with-open [wtr (clojure.java.io/writer output)]
      (doseq [[lat long] (repeatedly s (partial coords/generate-lat-long
                                                [[56.861786 66.416012]
                                                 [2.13232 143.232418]]))]
        ;; (telnet/telnet-send-line test-socket (str "geo gpsSpoofer fix "
        ;;                                           lat " " long))
        (http/post "http://127.0.0.1:8081/geo-fix"
                   {:form-params {:lat lat :long long}})
;        (fn-refresh)
        (print  "REFRESHED: " lat long)
        (let [[la lo] (time (core/start-attack (assoc conf :user user)))]
          (.write wtr (str [[lat long]
                            [la lo]
                            (coords/lat-long-distance lat long la lo)]
                           "\n")))))))

(defmacro print-and-return [expr]
  (let [x (gensym)]
    `(let [~x ~expr]
       (print ~x)
       ~x)))

(defn point-by-dim [point distance rot]
  (let [[x y zone zone-letter] (apply coords/ll->utmxy point)
        radians (Math/toRadians rot)]
    (coords/utmxy->ll (+ x (* (Math/cos radians) distance))
                      (+ y (* (Math/sin radians) distance))
                      zone
                      zone-letter)))

(defn make-precision-test [{:keys [host port output fn-refresh user conf]}]
;  (core/prepare-device! conf)
  (let [test-socket (doto (Socket.)
                        (.connect (InetSocketAddress. host port) 3000)
                        (telnet/telnet-read-line)
                        (telnet/telnet-send-line "geo gpsSpoofer start"))]
    (fn [start-pt end-pt n]
      (let [points (-> {:path [start-pt end-pt] :samples n}
                       google/get-google-elevation-data
                       google/location-points)
            [lat long] (first points)]
        (telnet/telnet-send-line test-socket (str "geo gpsSpoofer fix "
                                                  lat " " long))
        (fn-refresh)
        (let [new-conf (assoc conf :user user)]
          (with-open [wtr (clojure.java.io/writer output)]
            (doseq [p points]
              (print p)
              (.write wtr
                      (print-and-return
                       (str (core/new-point-read p new-conf)
                            " "
                            p
                            " "
                            (apply coords/lat-long-distance lat long p)
                            "\n"))))))))))
