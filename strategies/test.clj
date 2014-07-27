(defn assert [cond]
  (if cond
    (trace 0)
    (trace -1)))

(defn main [x y]
  (let [l (list 1 2 3 4)
        k (list 5 6 7)
        test-length (assert (= 4 (length l)))
        test-map (trace (map (fn [x] (+ x x)) l))
        test-filter (trace (filter (fn [x] (< 3 x)) l))
        test-reverse (trace (reverse l))
        test-foldi-left (trace (foldi-left (fn [i acc x]
                                             (pair (* i x) acc)) 0 l))
        test-append (trace (append l k))
        test-concat (trace (concat (list l k l)))
        test-min-by (assert (= -3 (min-by (fn [x] (* x x))
                                          (list 20 -3 -10 10 ))))]
    l))
