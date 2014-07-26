(defn trace [x] (debug x x))

(defn mod [x y]
  (- x (* y (/ x y))))

(defn random [seed max]
  (let [im 139968
        new-seed (mod (+ (* seed 3877) 29573) im)]
    (pair new-seed (/ (* max seed) im))))

(defn main [foo bar]
  (let [max 10
        p0 (trace (random 42 max))
        p1 (trace (random (head p0) max))
        p2 (trace (random (head p1) max))]
    p2))
