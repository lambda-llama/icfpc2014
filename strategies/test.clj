(defn trace [x] (debug x x))

(defn empty? [list] (atom list))

(defn fold-left [f acc list]
  (if (empty? list)
    acc
    (fold-left f (f acc (head list)) (tail list))))

(defn fold-right [f acc list]
  (if (empty? list)
    acc
    (f (head list) (fold-right f acc (tail list)))))

(defn map [f list]
  (fold-right (fn [x acc] (pair (f x) acc)) 0 list))

(defn filter [pred list]
  (fold-right (fn [x acc]
                (if (pred x) (pair x acc) acc))
              0 list))

(defn length [list]
  (fold-left (fn [acc x] (+ acc 1)) 0 list))

(defn main [x y]
  (let [l (list 1 2 3 4)
        test-length (trace (length l))
        test-map (trace (map (fn [x] (+ x x)) l))
        test-filter (trace (filter (fn [x] (< 3 x)) l))]
    l))
