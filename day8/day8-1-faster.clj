; Initial solution, but the approach did not have much in common with part 2
; Furter, clojure.core.matrix was not available with babashka, which I wanted to use
; But hey, its O(n^2)

(require  '[clojure.string :as str])

(def input (slurp  "day8/test.txt"))

(defn parse-digit [digit]
  (Character/digit digit 10))

(defn parse-grid [input]
  (->> input
       str/split-lines
       (map #(map parse-digit %))))

(defn visible-from-left [trees]
  (second
   (reduce
    (fn [[highest trees] tree]
      [(max highest tree) (conj trees (> tree highest))])
    [(first trees) [true]]
    (rest trees))))

(defn elem-or [a b]
  (mapv #(or %1 %2) a b))

(defn visible-in-row [trees]
  (let [visible-from-lefts (visible-from-left trees)
        visible-from-rights (rseq (visible-from-left (rseq (vec trees))))]
    (elem-or visible-from-lefts visible-from-rights)))

(defn transpose [m]
  (apply mapv vector m))

(defn visible-in-grid [grid]
  (let [visible-from-top (map visible-in-row grid)
        visible-from-bottom (transpose (map visible-in-row (transpose grid)))]
    (mapv elem-or visible-from-top visible-from-bottom)))


(->> input
     parse-grid
     visible-in-grid
     flatten
     (filter true?)
     count)
