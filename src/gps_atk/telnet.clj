(ns gps_atk.telnet
  (:require [clojure.java.io :as io])
  (:import [java.net Socket InetSocketAddress]))

(def gl-socket (atom nil))

(defn telnet-read-line
  "Read one line from telnet"
  ([]
     (telnet-read-line @gl-socket))
  ([^Socket socket]
     (let [rdr (io/reader (.getInputStream socket))]
       (.readLine rdr))))

(defn telnet-send-line
  "Send one line to telnet"
  ([line]
     (telnet-send-line @gl-socket line))
  ([^Socket socket line]
     (.write (.getOutputStream socket) (.getBytes (str line "\n") "UTF-8"))))

(defn telnet-connect!
  "Connect to telnet client, returns java socket"
  [^String hostname ^long port]
  (reset! gl-socket (doto (Socket.)
                      (.connect (InetSocketAddress. hostname port) 3000)
                      (telnet-read-line)
                      (telnet-send-line "geo gpsSpoofer start"))))

(defn telnet-disconnect!
  "Disconnect from client"
  []
  (.close ^Socket @gl-socket))
