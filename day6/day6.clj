(def input (slurp "day6/input"))

(defn contains-duplicates? [s]
  (not (apply distinct? s)))

; Part 1

(->> input
     (partition 4 1)
     (take-while contains-duplicates?)
     (count)
     (+ 4))

(->> input
     (partition 14 1)
     (take-while contains-duplicates?)
     (count)
     (+ 14))

