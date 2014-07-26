(defn nth [list pos]
  (if (== pos 0) (head list) (nth (tail list) (pred pos)))

(defn succ [x]
  (add x 1))

(defn pred [x]
  (sub x 1))

(defn lookup [list elem]
  (let [lookup-in (fn [l p]
                  (if (== (head l) elem)
                    p
                    (lookup-in (tail l) (succ p))))]
    (lookup-in list 0)))
