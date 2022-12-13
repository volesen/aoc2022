(require '[clojure.string :as str])

(def input (slurp "day13/input.txt"))

(def parsed
  (for [pairs (str/split input #"\n\n")]
    (for [packet (str/split-lines pairs)]
      (read-string packet))))

(defn order [left right]
  (cond
    (and (number? left) (number? right))
    (compare left right)

    (and (number? left) (sequential? right))
    (order [left] right)

    (and (sequential? left) (number? right))
    (order left [right])

    (and (empty? left) (empty? right)) 0
    (empty? left) -1
    (empty? right) 1

    :else
    (let [a (first left) b (first right)]
      (case (order a b)
        0 (order (rest left) (rest right))
        (order a b)))))

(defn filter-indexes [pred coll]
  (keep-indexed (fn [i x] (when (pred x) (inc i))) coll))

; Part 1
(->> parsed
     (filter-indexes (fn [[left right]]
                       (= (order left right) -1)))
     (reduce +))

; Part 2
(def divider-packets #{[[2]] [[6]]})

(->> parsed
     (apply concat)
     (concat divider-packets)
     (sort order)
     (filter-indexes #(divider-packets %))
     (reduce *))
