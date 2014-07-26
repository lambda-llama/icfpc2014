(defn nth [list pos]
  (if (== pos 0) (head list) (nth (tail list) (- pos 1)))
