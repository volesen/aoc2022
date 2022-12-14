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

(defn expand-line [points]
  (for [[[x1 y1] [x2 y2]] (partition 2 1 points)
        x (between x1 x2)
        y (between y1 y2)]
    [x y]))

(def rocks
  (->> parsed
       (mapcat expand-line)
       set))

(defn simulate-particle [bottom settled]
  (loop [[x y] [500 0]]
    (cond
      (>= y bottom) [x y]

      ; Go down
      (not (contains? (set settled) [x (inc y)]))
      (recur [x (inc y)])

      ; Go left
      (not (contains? (set settled) [(dec x) (inc y)]))
      (recur [(dec x) (inc y)])

      ; Go right
      (not (contains? (set settled) [(inc x) (inc y)]))
      (recur [(inc x) (inc y)])

      ; Settle
      :else [x y])))

(defn simulate-particles [stop? bottom settled]
  (loop [grid settled]
    (let [particle (simulate-particle bottom grid)]
      (if (stop? particle)
        grid
        (recur (conj grid particle))))))

; Part 1
(let [abyss (apply max (map first rocks))
      stop? #(>= (second %) abyss)]
  (-> (simulate-particles stop? abyss rocks)
      (set/difference rocks)
      count))

; Part 2  
(let [floor (inc (apply max (map second rocks)))
      stop? #(= % [500 0])]
  (-> (simulate-particles stop? floor rocks)
      (set/difference rocks)
      count
      inc)) ; Include the particle that settled at the top