(let [lookup (fn [lst elem]
               (let [lookup-in (fn [l p]
                                 (if (= (head l) elem)
                                   p
                                   (lookup-in (tail l) (inc p))
                                   ))]
                 (lookup-in lst 0)))]
  (lookup (list 1 2 3 4 5 6 7) 4))
