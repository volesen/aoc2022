(require '[clojure.string :as str])
(require '[clojure.core.match :refer [match]])

(def input (slurp "day7/input.txt"))

(defn step [{:keys [pwd fs]} command]
  (match (str/split command #" ")
    ["$" "ls"] {:pwd pwd :fs fs}

    ["$" "cd" dir] {:pwd (case dir
                           "/"  []
                           ".." (pop pwd)
                           (conj pwd dir))
                    :fs fs}

    ["dir" name] {:pwd pwd
                  :fs (update-in fs (conj pwd name) #(or % {}))}

    [size name] {:pwd pwd
                 :fs (assoc-in fs (conj pwd name) (parse-long size))}))

(defn parse-fs [commands]
  (->> commands
       str/split-lines
       (reduce step {:pwd [] :fs {}})
       :fs))

(defn dir-size [root dir]
  (reduce-kv
   (fn [{:keys [name size subdirs]} k v]
     (if (map? v)
       (let [sub-size (dir-size k v)]
         {:name name :size (+ size (:size sub-size)) :subdirs (conj subdirs sub-size)})
       {:name name :size (+ size v) :subdirs subdirs}))
   {:name root :size 0 :subdirs []}
   dir))


; Part 1
(def dir-sizes
  (->> input
       parse-fs
       (dir-size "/")))

(->> dir-sizes
     (tree-seq map? :subdirs)
     (filter #(< (:size %) 100000))
     (map :size)
     (reduce +))

; Part 2
(let [used (:size dir-sizes)
      to-delete (- used 40000000)]
  (->> dir-sizes
       (tree-seq map? :subdirs)
       (filter #(> (:size %) to-delete))
       (sort-by :size)
       first))
