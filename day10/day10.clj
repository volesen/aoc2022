(require '[clojure.string :as str])
(require '[clojure.core.match :refer [match]])

(def input (slurp "day10/input.txt"))

(defn parse [input]
  (for [line (str/split-lines input)
        :let [instruction (str/split line #" ")]]
    (match instruction
      ["noop"] [:noop]
      ["addx" n] [:addx (parse-long n)])))

(defn run [instructions]
  (reduce (fn [states instruction]
            (let [register (last states)]
              (match instruction
                [:noop] (concat states [register])

                ; Takes two cycles to add
                [:addx n] (concat states [register
                                          (+ register n)]))))
          [1]
          instructions))

; Part 1
(let [instructions (parse input)
      signal (run instructions)]
  (->> (for [cycle (range 20 (inc 220) 40)
             :let [value (nth signal (dec cycle))]]
         (* cycle value))
       (reduce +)))


; Part 2
(let [instructions (parse input)
      signal (run instructions)]
  (->> (for [y (range 6)]
         (for [x (range 40)
               :let [cycle (+ (* y 40) x)
                     horizontal-pos (nth signal cycle)
                     draw? (<= -1 (- x horizontal-pos) 1)]]
           (if draw? "#" ".")))
       (map str/join)))

