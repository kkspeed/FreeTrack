(ns gps-atk.schedule

  (:import [java.net URLEncoder])
  (:require [gps_atk.dsl :as dsl]
            [gps-atk.core :as core]
            [gps-atk.server :as server]
            [gps-atk.lib.coords :as coords]
            [gps-atk.debug :as debug]
            [gps-atk.utils :as utils]
            [gps-atk.device :as device]
            [clojure.java.shell :as sh]
            [clojure.tools.nrepl.server :as nrepl]
            [clj-http.client :as http]))

(defonce =conf= (atom nil))

(defonce =pause= (atom nil))

(defn pause [] (reset! =pause= true))

(defn resume []
  (reset! =pause= false)
  (future (dsl/start :run-task {:conf @=conf=})))

(defn paused? [s] @s)

(defn save-conf [c] (reset! =conf= c))

(defn request-task [{:keys [server-url app]}]
  (let [{:keys [state] :as task} (->> {:form-params {:app (name app)}}
                                      (http/post server-url)
                                      (:body)
                                      load-string)]
    (when (> state -1)
      (dissoc task :state :status))))

(dsl/defstate :stopped state
  (println "Stopped!"))

(dsl/defstate :run-task state
  (if (not (paused? =pause=))
    (do
      (if-let [task (request-task (:conf state))]
        (do
          (println "Fetched: " task)
          (try
            (let [cur-conf (merge (:conf state) task
                                  {:start-time (utils/timestamp)})
                  _ (debug/set-log-parameters cur-conf)
                  _ (save-conf cur-conf)
                  [lat1 long1] (core/start-attack cur-conf)
                  {:keys [lat long]} task
                  {:keys [server-submit-url app]} (:conf state)]
              (->> {:form-params
                    (merge task {:ilat lat1 :ilong long1
                                 :dist (coords/lat-long-distance lat long
                                                                 lat1 long1)
                                 :app (name app)
                                 :status "success"})}
                   (http/post server-submit-url)))
            (catch Exception e
              (->> {:form-params
                    (merge task {:ilat -1.0 :ilong -1.0
                                 :dist -1.0
                                 :app (name (get-in state [:conf :app]))
                                 :status
                                 (condp re-seq (or (.getMessage e) "fail")
                                   #"too old: " "old"
                                   #"too new: " "queue"
                                   "fail")})}
                   (http/post (get-in state [:conf :server-submit-url])))
              (when-let [recover-ops (get-in state [:conf :recover])]
                (debug/log-info "Recovering... ")
                (device/eval-op-seq recover-ops (assoc (:conf state) :user
                                                  (get-in state
                                                          [:conf :user-re]))))
              (and e (debug/log-exception e)))))
        (do (println "Wait!")
            (Thread/sleep 30000)))
      state)
    (assoc state :cur :stopped)))

(defn start-task [conf]
  (core/prepare-device! conf)
  (dsl/start :run-task {:conf conf}))

(defn -main [conf-file & [nrepl-port]]
  (when nrepl-port
    (nrepl/start-server :port (Integer/parseInt nrepl-port)))
  (start-task (load-file conf-file)))
