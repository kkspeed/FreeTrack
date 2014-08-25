(ns gps-atk.lib.octave
  (:import [dk.ange.octave OctaveEngineFactory]
           [dk.ange.octave.type Octave OctaveString OctaveDouble]))

(defn octave-eval [o statement & {:keys [var]}]
  (.eval o statement)
  (if var
    (vec (.getData (.get o OctaveDouble var)))))

(defn get-octave-function []
  (let [octave (.getScriptEngine (OctaveEngineFactory.))]
    (partial octave-eval octave)))

(defmacro with-octave [o & body]
  `(let [~o (.getScriptEngine (OctaveEngineFactory.))]
     ~(let [r (gensym)]
        `(let [~r (do ~@body)]
           (.close ~o)
           ~r))))

;; (.eval octave "pkg load optim")
;; (.eval octave "x = 0:0.01:1;")
;; (.eval octave "t = lsode('x**2+t**2', 0, x);")
;; (def result  (.get octave OctaveDouble "t"))
