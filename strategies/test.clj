(defn assert [cond]
  (if cond
    92
    (trace 11111111)))

(defn main [x y]
  (let [l (list 1 2 3 4)
        test-length (assert (= 4 (length l)))
        test-map (trace (map (fn [x] (+ x x)) l))
        test-filter (trace (filter (fn [x] (< 3 x)) l))
        test-min-by (assert (= -3 (min-by (fn [x] (* x x))
                                          (list 20 -3 -10 10 ))))]
    l))
