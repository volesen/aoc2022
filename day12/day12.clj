(require '[clojure.string :as str])

(def input (slurp "day12/input.txt"))

(defn indexed [grid]
  (for [row (range (count grid))
        col (range (count (first grid)))]
    [row col (get-in grid [row col])]))

(defn height [c]
  (- (int c) 96))

(defn parse [input]
  (let [grid (indexed (str/split-lines input))]
    (reduce (fn [map [x y c]]
              (case c
                \S (-> map
                       (assoc-in [:start] [x y])
                       (assoc-in [:heights [x y]] 1))
                \E (-> map
                       (assoc-in [:end] [x y])
                       (assoc-in [:heights [x y]] 26))
                (assoc-in map [:heights [x y]] (height c))))
            {:start nil :end nil :heights {}}
            grid)))

(defn steppable? [from to]
  (>= (inc from) to))

(defn neighbors [heights [x y]]
  (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [coord [(+ x dx) (+ y dy)]
              height (heights [x y])
              neigbour (heights coord)]
        :when (and neigbour (steppable? height neigbour))]
    coord))

(defn shortest-path [heights start end]
  (loop [queue (list start)
         distances {start 0}]
    (if-let [current (first queue)]
      (let [distance (get distances current)]
        (if (= current end)
          distance
          (let [next (for [neighbor (neighbors heights current)
                           :when (not (contains? distances neighbor))]
                       [neighbor (inc distance)])
                new-queue (concat (rest queue) (map first next))
                new-seen (into distances next)]
            (recur new-queue new-seen))))
      ##Inf)))

; Part 1
(let [{:keys [start end heights]} (parse input)]
  (shortest-path heights start end))

; Part 2
; Could have done BFS from end, but I was lazy
(let [{:keys [_ end heights]} (parse input)]
  (->> (for [start (keys heights)
             :when (= (heights start) 1)]
         (shortest-path heights start end))
       (apply min)))