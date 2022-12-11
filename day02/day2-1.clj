(require '[clojure.string :as str])

(def input (slurp "day02/input.txt"))

; Parsing
(def parse-moves
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(defn parse-match [line]
  (map parse-moves (str/split line #"\s")))

(defn parse-input [input]
  (map parse-match (str/split-lines input)))

; Logic
(def beats
  {:rock :scissors
   :paper :rock
   :scissors :paper})

(defn outcome [player opponent]
  (cond
    (= player opponent) :draw
    (= (beats player) opponent) :won
    :else :lost))

(def move-score
  {:rock 1
   :paper 2
   :scissors 3})

(def match-score
  {:lost 0
   :draw 3
   :won 6})

(defn score [[opponent player]]
  (+ (move-score player) (match-score (outcome player opponent))))

(->> input
     (parse-input)
     (map score)
     (reduce +))
