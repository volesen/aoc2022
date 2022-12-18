(require '[clojure.string :as str])

(def input (slurp "day16/input.txt"))


(def valves
  (into
   {}
   (for [line (str/split-lines input)
         :let [[_ valve flow tunnels] (re-matches #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)" line)]]
     [valve {:flow (parse-long flow)
             :tunnels (str/split tunnels #", ")}])))

; Calculate pairwise distances
(def distances
  (reduce-kv
   (fn [distances valve {:keys [tunnels]}]
     (-> distances
         (assoc [valve valve] 0)
         (into (for [tunnel tunnels] [[valve tunnel] 1]))))
   {}
   valves))

;Floyd–Warshall algorithm
(def distances
  (let [kij (for [k (keys valves)
                  i (keys valves)
                  j (keys valves)]
              [k i j])]
    (reduce
     (fn [distances [k i j]]
       (assoc distances [i j]
              (min (get distances [i j] ##Inf)
                   (+ (get distances [i k] ##Inf)
                      (get distances [k j] ##Inf)))))
     distances
     kij)))

(defn max* [coll]
  (when (seq coll)
    (apply max coll)))

(def non-zero-valves (filter (fn [[_ v]] (pos? (:flow v))) valves))

(defn max-flow* [t opened-valves valve]
  (let [flow (get-in valves [valve :flow])
        relieved-pressure (* flow t)
        next-valves (for [next-valve (keys non-zero-valves)
                          :let [distance (get distances [valve next-valve])]
                          :when (and (> t distance) (not (contains? opened-valves next-valve)))]
                      (max-flow* (- t distance 1) (conj opened-valves next-valve) next-valve))]
    (+ relieved-pressure
       (or (max* next-valves) 0))))

(defn max-flow [& args]
  (with-redefs [max-flow* (memoize max-flow*)]
    (apply max-flow* args)))

(time (max-flow 30 #{} "AA"))

; Part 2

(defn max-flow*
  [t not-opened valve elephant?]
  (let [flow (get-in valves [valve :flow])
        relieved-pressure (* flow t)
        next-valves (for [next-valve not-opened
                          :let [distance (distances [valve next-valve])]
                          :when (> t distance)]
                      (max-flow* (- t distance 1) (disj not-opened next-valve) next-valve elephant?))
        with-elephant (if elephant? (conj next-valves (max-flow* 26 not-opened "AA" false)) next-valves)]
    (+ relieved-pressure
       (or (max* with-elephant) 0))))


(def non-zero-valves (->> valves
                          (filter (fn [[_ v]] (pos? (:flow v))))
                          keys
                          set))

(time (println (max-flow 26 non-zero-valves "AA" true)))