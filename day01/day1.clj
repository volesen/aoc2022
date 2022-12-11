(require '[clojure.string :as str])

(def input (slurp "day01/input.txt"))

; Part 1

(defn parse-calories [block]
  (map #(Integer/parseInt %) (str/split-lines block)))

(defn parse-input [input]
  (map parse-calories (str/split input  #"\n\n")))

(defn take-largest [n items]
  (take-last n (sort items)))

(defn sum [calories]
  (reduce + calories))


(->> input
     (parse-input)
     (map sum)
     (take-largest 1))

; Part 2

(->> input
     (parse-input)
     (map sum)
     (take-largest 3)
     (sum))
