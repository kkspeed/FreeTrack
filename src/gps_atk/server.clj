(ns gps-atk.server
  (:import [java.util Date]
           [java.text SimpleDateFormat]
           [java.util.concurrent LinkedBlockingQueue BlockingQueue])
  (:use [ring.adapter.jetty :only (run-jetty)]
        [compojure.core])
  (:require [clojure.data.json :as json]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.java.shell :as sh]
            [gps_atk.telnet :as telnet]
            [gps-atk.device :as device]))

(defn- get-distance [env conf]
  (:distance (device/eval-op-seq (:fetch conf) env)))

(defn- new-point-read [[lat long] conf]
  (telnet/telnet-send-line (str "geo gpsSpoofer fix " lat " " long))
  (device/eval-op-seq (:refresh conf) nil))

(defonce =conf= (atom {}))

(defonce coords (atom {}))

(defonce server (atom {}))

(defonce work-queue (LinkedBlockingQueue.))

(defn add-task [task]
  (.put work-queue task))

(defn take-task []
  (.take work-queue))

(def services
  {:baidu (fn []
            {:headers {"Content-Type" "text/plain"}
             :body (json/write-str {:content {:addr {:detail ""}
                                              :bldg ""
                                              :floor ""
                                              :point @coords
                                              :radius "70.0"}
                                    :result {:error 161
                                             :time (-> (SimpleDateFormat.
                                                        "yyyy-MM-dd HH:mm:ss")
                                                       (.format (Date.))
                                                       (.toString))}})})})

(defn set-coords [lat long]
  (swap! coords merge {:x (str long) :y (str lat)}))

(defroutes app*
  (POST "/submit" [lat long user] (do (add-task {:user user
                                                 :real {:lat lat :long long}})
                                      {:status 200 :headers {} :body ""}))
  (GET  "/" [] "<h1>Hello</h1>")
  (POST "/sdk.php" [] ((:baidu services)))
  (GET  "/sdk.php" [] ((:baidu services)))
  (POST "/geo-fix" [lat long] {:status 200
                               :headers {}
                               :body (json/write-str
                                      {:message "ok"
                                       :dist (new-point-read
                                              [(Float/parseFloat lat)
                                               (Float/parseFloat long)]
                                              @=conf=)})})
  (GET "/refresh" [] (do
                       (future (device/eval-op-seq (:refresh @=conf=) @=conf=))
                       {:status 200
                        :headers {}
                        :body (json/write-str {:message "ok"})})))

(def app (compojure.handler/api app*))

(defn start-server! [s port]
  (reset! server (run-jetty s {:port port :join? false})))

(defn run-location-service! [{:keys [location-service-port] :as conf}]
  (swap! =conf= merge conf)
  (start-server! app location-service-port))

(defn stop-service! []
  (.stop @server))
