(ns gps_atk.dsl)

(defmulti transit
  "From one automata state to the other"
  (fn [state] (:cur state)))

(defmethod transit nil
  [state]
  state)

(defmacro defstate
  "Define a state of the attack automata"
  [type & body]
  `(defmethod transit ~type
     [~(first body)]
     #(transit (do ~@(rest body)))))

(defn start
  "Define the start point of the automata"
  [init-state values]
  (trampoline  (transit (assoc values :cur init-state))))