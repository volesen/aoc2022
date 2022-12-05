(require '[clojure.string :as str])

(def input (slurp "day5/input"))

(defn parse-move [move]
  (let [[_ n _ from _ to] (str/split move #" ")]
    [(parse-long n)
     (dec (parse-long from))
     (dec (parse-long to))]))

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
    [(into [] (parse-stacks stacks)) (parse-moves moves)]))

; Logic

(defn pop-n [stack n]
  (let [[popped rest] (split-at n stack)]
    [(reverse popped) rest]))

(defn play-move [stacks [n from to]]
  (let [from-stack (nth stacks from)
        to-stack (nth stacks to)
        [popped rest] (pop-n from-stack n)]
    (assoc stacks
           from rest
           to (concat popped to-stack))))

(defn play-moves [[stacks moves]]
  (reduce play-move stacks moves))

(defn extract-top [stacks]
  (map first stacks))

; Part 1

(->> input
     parse
     play-moves
     extract-top)

; Part 2

(defn play-move [stacks [n from to]]
  (let [from-stack (nth stacks from)
        to-stack (nth stacks to)
        [popped rest] (split-at n from-stack)] ; Key difference
    (assoc stacks
           from rest
           to (concat popped to-stack))))

(->> input
     parse
     play-moves
     extract-top)
