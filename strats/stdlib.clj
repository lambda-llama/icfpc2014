(defn nth [list pos]
  (if (== pos 0) (head list) (nth (tail list) (pred pos)))

(defn succ [x]
  (add x 1))

(defn pred [x]
  (sub x 1))
