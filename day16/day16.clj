(require '[clojure.string :as str])

(def input (slurp "day16/input.txt"))

(def valves
  (reduce
   (fn [valves line]
     (let [[_ valve flow tunnels] (re-matches #"Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)" line)]
       (assoc valves valve {:flow (parse-long flow)
                            :tunnels (str/split tunnels #", ")})))
   {}
   (str/split-lines input)))

(defn init-distances [valves]
  (let [selfs (for [[valve _] valves]
                [[valve valve] 0])
        neighbours (for [[valve info] valves
                         tunnel (:tunnels info)]
                     [[valve tunnel] 1])]
    (into {} (concat selfs neighbours))))

(defn pairwise-shortest-distance
  "Floyd–Warshall algorithm"
  [valves]
  (reduce
   (fn [distances [k i j]]
     (assoc distances [i j]
            (min (get distances [i j] ##Inf)
                 (+ (get distances [i k] ##Inf)
                    (get distances [k j] ##Inf)))))
   (init-distances valves)
   (for [k (keys valves)
         i (keys valves)
         j (keys valves)]
     [k i j])))

(def distances (pairwise-shortest-distance valves))

(defn max* [coll]
  (if (seq coll)
    (apply max coll)
    0))

; Without the elephant helping, this is a classic knapsack problem.
; With the elephant, we optimize at each t the optimal remaining valves to open (we can stop early),
; taking into conseration that the elephant can optimally open the remaining set of valves starting,
; with 26 minutes left.
(defn max-flow* [t not-opened valve elephant?]
  (let [flow (get-in valves [valve :flow])
        next-valves* (for [next-valve not-opened
                          :let [distance (distances [valve next-valve])]
                          :when (> t distance)]
                      (max-flow* (- t distance 1) (disj not-opened next-valve) next-valve elephant?))
        next-valves (if elephant?
                        (conj next-valves* (max-flow* 26 not-opened "AA" false))
                        next-valves*)]
    (+ (* flow t) (max* next-valves))))

(defn max-flow [t not-opened valve elephant?]
  (with-redefs [max-flow* (memoize max-flow*)]
    (max-flow* t not-opened valve elephant?)))

(def non-zero-valves (->> valves
                          (filter (fn [[_ v]] (pos? (:flow v))))
                          keys
                          set))

; Part 1
(max-flow 30 non-zero-valves "AA" false)

; Part 2
(max-flow 26 non-zero-valves "AA" true)