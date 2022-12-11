(require '[clojure.string :as str])

(def input (slurp "day11/input.txt"))

; Parsing
(defn parse-operation [op num]
  (fn [old] (let [op (case op "+" + "*" *)
                  num (or (parse-long num) old)]
              (op old num))))

(defn parse-monkey [block]
  (let [[_ items] (re-find #"Starting items: (.*)" block)
        [_ op num] (re-find #"Operation: new = old (\+|\*) (\d+|old)" block)
        [_ divisor] (re-find #"Test: divisible by (\d+)" block)
        [_ if-true] (re-find #"If true: throw to monkey (\d+)" block)
        [_ if-false] (re-find #"If false: throw to monkey (\d+)" block)]
    {:items (vec (map parse-long (str/split items #", ")))
     :update (parse-operation op num)
     :divisor (parse-long divisor)
     :throw-true (parse-long if-true)
     :throw-false (parse-long if-false)
     :inspections 0}))

(defn parse [input]
  (for [block (str/split input #"\n\n")]
    (parse-monkey block)))

; Logic
(defn divides? [num divisor]
  (zero? (mod num divisor)))

(defn round [normalize monkeys]
  (loop [monkey-id 0
         monkeys monkeys]
    (if-let [{:keys [items update divisor throw-true throw-false]} (get monkeys monkey-id)]
      (->> items
           (reduce
            (fn [worry-levels worry]
              (let [new-worry (normalize (update worry))
                    throw-to (if (divides? new-worry divisor) throw-true throw-false)]
                (-> worry-levels
                    (update-in [monkey-id :items] pop)
                    (update-in [monkey-id :inspections] inc)
                    (update-in [throw-to :items] conj new-worry))))
            monkeys)
           (recur (inc monkey-id)))
      monkeys)))


(defn calculate-monkey-business [monkeys]
  (->> monkeys
       (map :inspections)
       sort
       (take-last 2)
       (reduce *)))

; Part 1
(let [monkeys (vec (parse input))
      divide-by-3 (fn [num] (quot num 3))
      rounds (iterate (partial round divide-by-3) monkeys)]
  (calculate-monkey-business (nth rounds 20)))

; Part 2 (divisors are all prime, which we can exploit)
(let [monkeys (vec (parse input))
      product (reduce * (map :divisor monkeys))
      normalize (fn [num] (mod num product))
      rounds (iterate (partial round normalize) monkeys)]
  (calculate-monkey-business (nth rounds 10000)))
