(require '[clojure.string :as str])

(def input (slurp "day9/input.txt"))

(defn parse [input]
  (for [line (str/split-lines input)
        :let [[dir dist] (str/split line #" ")]]
    [(keyword dir) (Integer/parseInt dist)]))

(defn step-head [dir [x y]]
  (case dir
    :R [(inc x) y]
    :L [(dec x) y]
    :U [x (inc y)]
    :D [x (dec y)]))

(defn chessboard-distance [[x1 y1] [x2 y2]]
  (max (Math/abs (- x1 x2))
       (Math/abs (- y1 y2))))

(defn step-tail [[x1 y1] [x2 y2]]
  (if (> (chessboard-distance [x1 y1] [x2 y2]) 1)
    [(+ x2 (compare x1 x2)) (+ y2 (compare y1 y2))]
    [x2 y2]))


(defn simulate-move [[head & rest] dir]
  (reduce (fn [rope tail]
            (let [prev (last rope)
                  moved (step-tail prev tail)]
              (conj rope moved)))
          [(step-head dir head)]
          rest))

(defn simulate [n moves]
  (reductions simulate-move (repeat n [0 0]) moves))

(defn explode-move [[dir dist]]
  (repeat dist dir))

(defn solution [n input]
  (->> input
       parse
       (mapcat explode-move)
       (simulate n)
       (map last)
       distinct
       count))

; Part 1
(solution 2 input)

; Part 2
(solution 10 input)