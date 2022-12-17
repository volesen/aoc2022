(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input (slurp "day15/input.txt"))

(defn parse-line [line]
  (let [[_ x1 y1 x2 y2] (re-find #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)" line)]
    [[(parse-long x1) (parse-long y1)]
     [(parse-long x2) (parse-long y2)]]))

(def parsed (map parse-line (str/split-lines input)))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn coverage [y-line [[x y :as sensor] beacon]]
  (let [dist (manhattan-distance sensor beacon)
        ; After moving down to the line
        ; the distance left to cover is
        dist-left (- dist (abs (- y-line y)))]
    (when (>= dist-left 0)
      [(- x dist-left) (+ x dist-left)])))

(defn inclusive-range [[x1 x2]]
  (range x1 (inc x2)))

; Part 1
(def devices-on-line
  (->> parsed
       (apply concat)
       (filter #(= (second %) 2000000))
       set
       count))

(- (->> parsed
        (map (partial coverage 2000000))
        (remove nil?)
        (mapcat inclusive-range)
        set
        count)
   devices-on-line)

; Part 2
(defn find-gap
  "Return the covered interval or the first gap"
  [ranges]
  (let [sorted (sort ranges)]
    (reduce
     (fn [[l1 r1] [l2 r2]]
       (if (> l2 (inc r1))
         (reduced (inc r1))
         [(min l1 l2) (max r1 r2)]))
     (first sorted)
     (rest sorted))))

(let [gaps (for [y (range 0 (inc 4000000))
                 :let [x (->> parsed
                              (map (partial coverage y))
                              (remove nil?)
                              find-gap)]
                 :when (and (number? x) (<= 0 x 4000000))]
             [x y])
      [x y] (first gaps)]
  (+ (* 4000000 x) y))

