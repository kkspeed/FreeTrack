(ns gps-atk.device
  (:require [gps-atk.debug :as debug]
            [clojure.string :as string]
            [clojure.java.shell :as sh])
  (:import [com.android.chimpchat.adb AdbBackend]
           [com.android.chimpchat.core TouchPressType]))

(defonce gl-device (atom {:backend nil :connection nil}))

(defn adb-get-device!
  ([] (adb-get-device! 150000 ".*"))
  ([^long timeout ^String pattern]
     (when (nil? (:backend @gl-device))
       (swap! gl-device assoc :backend (AdbBackend.)))
     (swap! gl-device merge
            {:connection (.waitForConnection (:backend @gl-device)
                                             timeout
                                             pattern)})))

(defn adb-abort-connection! []
  (.shutdown ^AdbBackend (:backend @gl-device)))

(defn send-touch
  "Send touch event on x y"
  [x y]
  (debug/suppress-err
   (.touch (:connection @gl-device) x y TouchPressType/DOWN_AND_UP)))

(defn send-press
  "Send key press"
  [^String key]
  (debug/suppress-err
   (.press (:connection @gl-device) key TouchPressType/DOWN_AND_UP)))

(defn send-numpad-number [^String numbers]
  (doseq [key (map (partial str "KEYCODE_NUMPAD_") numbers)]
    (send-press key)))

(defn send-drag
  "Send drag event from x1 y1 to x2 y2"
  [x1 y1 x2 y2 & {:keys [mid length]}]
  (debug/suppress-err
   (.drag (:connection @gl-device) x1 y1 x2 y2
          (or mid 10) (or length 2400))))

(defn scroll [times [x1 y1 x2 y2]]
  (dotimes [x times]
    (send-drag x1 y1 x2 y2 :mid 5 :length 2)))

(defn shell-command [cmd]
  (.shell (:connection @gl-device) cmd))

(defn ocr-screenshot
  "Take a screenshot and then ocr the text"
  [x y width height]
  (sh/sh "adb-screenshot" "/tmp/ocr.png")
  (sh/sh "convert" "-sharpen" "5" "-crop"
         (str width "x" height "+" x "+" y)
         "/tmp/ocr.png" "/tmp/ocr-cropped.png")
  (sh/sh "tesseract" "/tmp/ocr-cropped.png" "/tmp/ocr" "-psm" "7")
  (:out (sh/sh "cat" "/tmp/ocr.txt")))

(defn stamp-time []
  (-> (shell-command "date +%s")
      (string/split #"\r")
      first (Integer/parseInt)))

(defmulti operate
  (fn [op & args] op))

(defmethod operate :touch [op env prev x y]
  (send-touch x y))

(defmethod operate :press [op env prev key]
  (send-press key))

(defmethod operate :drag [op env prev x1 y1 x2 y2]
  (send-drag x1 y1 x2 y2))

(defmethod operate :ocr [op env prev x y width height tailor]
  (tailor (ocr-screenshot x y width height)))

(defmethod operate :sleep [op env prev miliseconds]
  (Thread/sleep miliseconds))

(defmethod operate :raw [op env prev raw-fn & args]
  (apply raw-fn env prev args))

(defmethod operate :shell [op env prev {:keys [cmd args]}]
;  (prn cmd args)
  (shell-command
   (reduce (fn [c k]
             (string/replace c (str "#" k)
                             (str (k env))))
           cmd
           args)))

(defmethod operate :scroll [op env prev times coords]
  (scroll times coords))

(defmethod operate :timestamp [op env prev]
  (stamp-time))

(defmethod operate :input-num [op env prev nums-key]
  (send-numpad-number (nums-key env)))

(defn eval-op-seq [op-seq env]
  (reduce (fn [acc cur]
            (or (apply operate (let [[type & args] cur]
                                 (into [type env acc] args))) acc))
          nil
          op-seq))

(defn get-pid [app-pattern]
  (->> (shell-command "ps")
       (#(string/split %1 #"\r\n"))
       (filter #(re-find app-pattern %1))
       first
       (#(string/split %1 #"\s+"))
       second
       Integer/parseInt))
