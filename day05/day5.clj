(require '[clojure.string :as str])

(def input (slurp "day05/input.txt"))

(defn parse-move [move]
  (let [[_ n _ from _ to] (str/split move #" ")]
    [(parse-long n) (dec (parse-long from)) (dec (parse-long to))]))

(defn parse-moves [moves]
  (map parse-move (str/split-lines moves)))

(defn transpose [m]
  (apply mapv vector m))

; A bit of a hack, but it works
(defn parse-stacks [stacks]
  (for [stack (transpose (str/split-lines stacks))
        :when (Character/isDigit (last stack))]
    (filter #(Character/isLetter %) stack)))

(defn parse [input]
  (let [[stacks moves] (str/split input #"\n\n")]
    [(vec (parse-stacks stacks)) (parse-moves moves)]))

; Logic

(defn play-move [move-ordering stacks [n from to]]
  (let [from-stack (nth stacks from)
        to-stack (nth stacks to)
        [to-move rest] (split-at n from-stack)]
    (assoc stacks
           from rest
           to (concat (move-ordering to-move) to-stack))))

(defn play-moves [move-ordering [stacks moves]]
  (reduce (partial play-move move-ordering) stacks moves))

(defn extract-top [stacks]
  (map first stacks))

; Part 1
(->> input
     parse
     (play-moves reverse)
      extract-top)

; Part 2
(->> input
     parse
     (play-moves identity)
     extract-top)
