(require '[clojure.string :as str])

(def input (slurp "day2/input.txt"))

; Parsing

(def parse-move
  {"A" :rock
   "B" :paper
   "C" :scissors})

(def parse-outcome
  {"X" :lost
   "Y" :draw
   "Z" :won})

(defn parse-match [line]
  (let [[move outcome] (str/split line #"\s")]
    [(parse-move move) (parse-outcome outcome)]))

(defn parse-input [input]
  (map parse-match (str/split-lines input)))

; Logic
(def beats
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(def move-score
  {:rock 1
   :paper 2
   :scissors 3})

(def match-score
  {:lost 0
   :draw 3
   :won 6})

(defn strategy [opponent-move outcome]
  (cond
    (= outcome :draw) opponent-move
    (= outcome :lost) (beats opponent-move)
    :else (beats (beats opponent-move))))


(defn score [[opponent-move outcome]]
  (+ (move-score (strategy opponent-move outcome)) (match-score outcome)))


(->> input
     (parse-input)
     (map score)
     (reduce +))
