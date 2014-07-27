;; primitivza!

(defn trace [x] (debug x x))

;; is there a way to implement a lazzier AND?
(defn and [x y]
  (= (+ x y) 2))

(defn or [x y]
  (>= (+ x y) 1))

(defn mod [x y]
  (- x (* y (/ x y))))

(defn not= [a b]
  (not (= a b)))

(defn id [x]
  x)

(defn neg [x]
  (- 0 x))

;;
;; stdlibza!

(defn empty? [list] (atom list))

(defn fst [p] (head p))

(defn snd [p] (tail p))

(defn nth [list pos]
  (if (empty? list)
    list  ;; this way we can query tuples with '0' as last element.
    (if (= pos 0)
      (head list)
      (nth (tail list) (dec pos)))))

;; Returns the 0-based index of 'elem' in 'list'.
(defn index [list elem]
  (let [index-in (fn [l p]
                    (if (= (head l) elem)
                      p
                      (index-in (tail l) (inc p))))]
    (index-in list 0)))

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

(defn sum-by [f list]
  (fold-left (fn [acc x] (+ acc (f x))) 0 list))

(defn min-by [key list]
  (if (empty? list) (trace 667)
      (let [init-x (head list)
            init-weigth (key init-x)
            f (fn [x acc]
                (let [el (fst acc)
                      weight (snd acc)
                      x-weight (key x)]
                  (if (< x-weight weight)
                    (pair x x-weight)
                    acc)))]
        (head (fold-right f (pair init-x init-weigth) list)))))

(defn min [list]
  (min-by id list))

(defn reverse [list]
  (fold-left (fn [acc x] (pair x acc)) 0 list))

(defn length [list]
  (fold-left (fn [acc x] (+ acc 1)) 0 list))


;; http://benchmarksgame.alioth.debian.org/u32/performance.php?test=fasta#about
(defn random [seed max]
  (let [im 139968
        new-seed (mod (+ (* seed 3877) 29573) im)]
    (pair new-seed (/ (* max seed) im))))


(defn queue [] (pair 0 0))

(defn queue-push [q x]
  (let [front (fst q)
        back (snd q)]
    (pair front (pair x back))))

(defn queue-pop [q]
  (let [front (fst q)
        back (snd q)]
    (if (empty? front)
      (queue-pop (pair (reverse back) 0))
      (pair (head front) (pair (tail front) back)))))
