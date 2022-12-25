(require '[clojure.string :as str])

(def points
  (set
   (for [line (str/split-lines (slurp "day18/input.txt"))]
     (map parse-long (str/split line #",")))))

(defn neighbours [[x y z]]
  [[(inc x) y z]
   [(dec x) y z]
   [x (inc y) z]
   [x (dec y) z]
   [x y (inc z)]
   [x y (dec z)]])

; Part 1
(def part1
  (->> points
       (mapcat neighbours)
       (remove points)
       count))

; Part 2
(def side (apply max (flatten (seq points))))

(defn in-bounds? [[x y z]]
  (and (<= -1 x (inc side))
       (<= -1 y (inc side))
       (<= -1 z (inc side))))

(def part2
  "Returns the number of surfaces reachable from outside a cube surrounding the drop"
  (loop [stack [[0 0 0]]
         seen points
         surfaces 0]
    (if-let [point (peek stack)]
      (if (seen point)
        (recur (pop stack) seen surfaces)
        (recur (->> (neighbours point)
                    (filter in-bounds?)
                    (remove seen)
                    (into (pop stack)))
               (conj seen point)
               (->> (neighbours point)
                    (filter points)
                    (count)
                    (+ surfaces))))
      surfaces)))
