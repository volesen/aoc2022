(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input (slurp "day14/input.txt"))

(def parsed
  (for [rock-formation (str/split-lines input)]
    (for [point (str/split rock-formation #" -> ")
          :let [[x y] (str/split point #",")]]
      [(parse-long x) (parse-long y)])))

(defn between [x1 x2]
  (let [min (min x1 x2)
        max (max x1 x2)]
    (range min (inc max))))

(defn expand [points]
  (for [[[x1 y1] [x2 y2]] (partition 2 1 points)
        x (between x1 x2)
        y (between y1 y2)]
    [x y]))

(defn simulate-particle [bottom grid]
  (loop [[x y] [500 0]]
    (cond
      (>= y bottom) [x y]

      ; Go down
      (not (contains? (set grid) [x (inc y)]))
      (recur [x (inc y)])

      ; Go left
      (not (contains? (set grid) [(dec x) (inc y)]))
      (recur [(dec x) (inc y)])

      ; Go right
      (not (contains? (set grid) [(inc x) (inc y)]))
      (recur [(inc x) (inc y)])

      ; Settle
      :else [x y])))

(defn simulate-particles [stop? bottom grid]
  (loop [grid grid]
    (let [particle (simulate-particle bottom grid)]
      (if (stop? particle)
        grid
        (recur (conj grid particle))))))

(def grid
  (->> parsed
       (mapcat expand)
       set))

; Part 1
(let [abyss (apply max (map first grid))
      stop? #(>= (second %) abyss)]
  (-> (simulate-particles stop? abyss grid)
      (set/difference grid)
      count))

; Part 2  
(let [floor (inc (apply max (map second grid)))
      stop? #(= % [500 0])]
  (-> (simulate-particles stop? floor grid)
      (set/difference grid)
      count
      inc)) ; Include the particle that settled at the top