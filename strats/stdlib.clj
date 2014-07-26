(defn nth [list pos]
  (if (= pos 0)
    (head list)
    (nth (tail list) (dec pos))))

(defn lookup [list elem]
  (let [lookup-in (fn [l p]
                    (if (= (head l) elem)
                      p
                      (lookup-in (tail l) (inc p))))]
    (lookup-in list 0)))
