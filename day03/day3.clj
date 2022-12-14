(require '[clojure.string :as str])
(require '[clojure.set :as set])

(def input (slurp "day03/input.txt"))

(defn parse-compartments [line]
  (split-at (/ (count line) 2) line))

(defn priority [char]
  (cond
    (<= 97 (int char) 122) (- (int char) 96)
    (<= 65 (int char) 90) (- (int char) 38)))

(defn common-item [coll]
  (->> coll
       (map set)
       (reduce set/intersection)
       first))

; Part 1
(->> input
     str/split-lines
     (map parse-compartments)
     (map common-item)
     (map priority)
     (reduce +))


; Part 2

(->> input
     str/split-lines
     (partition 3)
     (map common-item)
     (map priority)
     (reduce +))

