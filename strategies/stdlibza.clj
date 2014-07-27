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

(defn abs [x]
  (if (> x 0)
    x
    (neg x)))

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

(defn contains? [list elem]
  (let [go (fn [acc]
             (if (empty? acc)
               0
               (if (= (head acc) elem)
                 1
                 (go (tail acc)))))]
    (go list)))

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

(defn foldi-left [f acc list]
  (snd (fold-left (fn [i-acc x]
                    (let [i (fst i-acc)
                          acc (snd i-acc)]
                      (pair (inc i) (f i acc x))))
                  (pair 0 acc) list)))

(defn fold-right [f acc list]
  (if (empty? list)
    acc
    (f (head list) (fold-right f acc (tail list)))))

(defn append [list1 list2]
  (if (empty? list1)
    list2
    (if (empty? list2)
      list1
      (fold-right (fn [x acc] (pair x acc)) list2 list1))))

(defn concat [lists]
  (fold-right append 0 lists))

(defn map [f list]
  (fold-right (fn [x acc] (pair (f x) acc)) 0 list))

(defn filter [pred list]
  (fold-right (fn [x acc]
                (if (pred x) (pair x acc) acc))
              0 list))

(defn sum-by [f list]
  (fold-left (fn [acc x] (+ acc (f x))) 0 list))

(defn sum [list]
  (sum-by id list))

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

(defn cumulative-sum [list]
  (tail
   (reverse
    (fold-left (fn [acc x] (pair (+ (head acc) x) acc))
               (pair 0 0)
               list))))

(defn first [pred list]
  (head (filter pred list)))

;; http://en.wikipedia.org/wiki/Linear_congruential_generator
(defn random [seed max]
  (let [im 139968
        ia 3877
        ic 29573
        new-seed (mod (+ (* seed ia) ic) im)]
    (pair new-seed (/ (* max seed) im))))


(defn queue [] (pair 0 0))

(defn queue-empty? [q]
  (and (empty? (fst q))
       (empty? (snd q))))

(defn queue-push [q x]
  (let [front (fst q)
        back (snd q)]
    (pair front (pair x back))))

(defn queue-push-all [q list]
  (let [front (fst q)
        back (snd q)]
    (pair front (append list back))))

(defn queue-pop [q]
  (let [front (fst q)
        back (snd q)]
    (if (empty? front)
      (queue-pop (pair (reverse back) 0))
      (pair (head front) (pair (tail front) back)))))


(defn set []
  0)

(defn set-contains? [s x]
  (contains? s x))

(defn set-add [s x]
  (if (set-contains? s x)
    s
    (pair x s)))
