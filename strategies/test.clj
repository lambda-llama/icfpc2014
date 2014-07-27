(defn main [x y]
  (let [l (list 1 2 3 4)
        test-length (trace (length l))
        test-map (trace (map (fn [x] (+ x x)) l))
        test-filter (trace (filter (fn [x] (< 3 x)) l))
        test-min-by (trace (min-by (fn [x] (- 10 x)) l))]
    l))
