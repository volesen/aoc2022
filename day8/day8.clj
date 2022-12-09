(require  '[clojure.string :as str])

(def input (slurp  "day8/input.txt"))

(defn parse-digit [digit]
  (Character/digit digit 10))

(defn parse-grid [input]
  (->> input
       str/split-lines
       (map #(map parse-digit %))))

(defn visible? [[left right above below] height]
  (or (every? #(< % height) left)
      (every? #(< % height) right)
      (every? #(< % height) above)
      (every? #(< % height) below)))

(defn transpose [grid]
  (apply mapv vector grid))

(defn grid-walk [f grid]
  (let [transposed (transpose grid)]
    (for [i (range (count grid))
          j (range (count transposed))
          :let [row (nth grid i)
                col (nth transposed j)
                [left [height & right]] (split-at j row)
                [above [_ & below]] (split-at i col)]]
      (f [(reverse left) right (reverse above) below] height))))

; Part 1
(->> input
     parse-grid
     (grid-walk visible?)
     (filter true?)
     count)

; Part 2
(defn score [direction height]
  (let [[lower rest] (split-with #(< % height) direction)]
    (if (empty? rest)
      (count lower)
      (+ 1 (count lower)))))

(defn scenic-score [directions height]
  (reduce * (map #(score % height) directions)))

(->> input
     parse-grid
     (grid-walk scenic-score)
     (apply max))
