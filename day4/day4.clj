(require '[clojure.string :as str])

(def input (slurp "day4/input"))


(defn parse-range
  "Parse a string of the form '123-456' into a range of integers."
  [s]
  (let [[a b] (str/split s #"-")]
    [(Integer/parseInt a) (Integer/parseInt b)]))

(defn parse-pair [pair]
  (map parse-range (str/split pair #",")))


; Logic

(defn fully-contained? [[[start1 end1] [start2 end2]]]
  (or  (and (<= start1 start2) (<= end2 end1))
       (and (<= start2 start1) (<= end1 end2))))


(defn overlaps? [[[start1 end1] [start2 end2]]]
  (or (<= start1 start2 end1)
      (<= start2 start1 end2)))

; Part 1
(->> input
     str/split-lines
     (map parse-pair)
     (filter fully-contained?)
     count)


; Part 2
(->> input
     str/split-lines
     (map parse-pair)
     (filter overlaps?)
     count)