(ns gps-atk.debug
  (:import [java.util Date]
           [java.text SimpleDateFormat])
  (:require [clj-http.client :as http]
            [clojure.stacktrace :as stacktrace]
            [clojure.pprint :as pprint]))

(def null-print-stream (-> "/dev/null"
                           (java.io.FileOutputStream.)
                           (java.io.PrintStream.)))

(defonce date-formatter (SimpleDateFormat. "yyyy-MM-dd HH:mm:ss z"))

(defmacro with-exception-debug [debug? e & body]
  (if debug?
    `(do ~(first body))
    `(try
       ~(first body)
       (catch Exception ~e ~@(second body)))))

(defmacro print-and-return [expr]
  (let [x (gensym)]
    `(let [~x ~expr]
       (prn ~x)
       ~x)))

(defmacro suppress-err [& body]
  (let [err (gensym)]
    `(let [~err System/err]
       (System/setErr null-print-stream)
       ~@body
       (System/setErr ~err))))

(def log-params (atom {}))

(defn make-logger [level]
  (fn [content]
    ((if (:url @log-params)
       #(http/post (:url @log-params)
                   {:form-params {:target (:id @log-params)
                                  :content (str %1)
                                  :time (.format date-formatter (Date.))
                                  :level level}})
        identity)
     (print-and-return content))
    content))

(def default-pt {:ptcnt 3
                 :pt1 {:lat 20
                       :lng 20
                       :dist 500000}
                 :pt2 {:lat 30
                       :lng 30
                       :dist 500000}
                 :pt3 {:lat 40
                       :lng 40
                       :dist 500000}
                 :gnd {:lat 43.002442
                       :lng -78.787729
                       :dist 0}})

(defn point-visualize [body]
  (http/post "http://localhost:8000/set-center"
             {:content-type :json
              :body (http/json-encode body)}))

(def log-error (make-logger "Error"))

(def log-info (let [logger (make-logger "Info")]
                (fn [c]
                  (if (and (map? c) (> (count (:ests c)) 3))
                    (if (or (= (:cur c) :momo-filter)
                            (= (:cur c) :done))
                      (let [[x y] (:center c)
                            posi (:possible c)]
                        (point-visualize
                         (assoc default-pt :ptcnt 1
                                :pt1 {:lat x
                                      :lng y
                                      :dist (float (/ (:radius c) (min (:x posi)
                                                                       (:y posi))))})))
                      (let [[p1 p2 p3] (take 3 (:ests c))
                            [x1 y1] (:coord p1)
                            [x2 y2] (:coord p2)
                            [x3 y3] (:coord p3)]
                        (point-visualize (assoc default-pt
                                           :pt1 {:lat x1
                                                 :lng y1
                                                 :dist (:dist p1)}
                                           :pt2 {:lat x2
                                                 :lng y2
                                                 :dist (:dist p2)}
                                           :pt3 {:lat x3
                                                :lng y3
                                                :dist (:dist p3)})))))
                  (logger c))))
(def log-warning (make-logger "Warning"))
(defn log-exception [^Exception e]
  (log-error
   (with-out-str (stacktrace/print-cause-trace e))))

(defn set-log-parameters [{:keys [log-url _id]}]
  (swap! log-params assoc :url log-url :id _id))
