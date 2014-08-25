(ns gps_atk.actions
  (:require [gps_atk.telnet :as telnet]
            [gps-atk.server :as server]
            [gps-atk.debug :as debug]
            [clj-http.client :as http]))

(defmulti send-location
  (fn [t & args] t))

(defmethod send-location :simulator [t latitude longitude]
  (send-location :dev-shell latitude longitude ""))

(defmethod send-location :dev-shell [t latitude longitude client]
  (debug/log-info (str "geo " client " fix "  latitude " " longitude))
  (telnet/telnet-send-line
   (str "geo " client " fix " latitude " " longitude))
  (Thread/sleep 1000))

(defmethod send-location :service [t latitude longitude]
  (debug/log-info (str "\033[1;36;44mservice: fix " latitude " " longitude "\033[m"))
  (server/set-coords latitude longitude)
  (Thread/sleep 500))

(defmethod send-location :device [t latitude longitude {:keys [url]}]
  (debug/log-info (str "\033[1;36;44mdevice: fix " latitude " " longitude "\033[m"))
  (http/get (str url "/set-loc/" latitude "/" longitude))
  (Thread/sleep 500))
