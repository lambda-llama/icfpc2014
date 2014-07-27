;; primitivza!

(defn trace [x] (debug x x))

;; is there a way to implement a lazzier AND?
(defn and [x y]
  (= (+ x y) 2))

(defn or [x y]
  (>= (+ x y) 1))

(defn mod [x y]
  (- x (* y (/ x y))))


;;
;; stdlibza!

(defn empty? [list] (atom list))

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

(defn length [list]
  (fold-left (fn [acc x] (+ acc 1)) 0 list))

;; http://benchmarksgame.alioth.debian.org/u32/performance.php?test=fasta#about
(defn random [seed max]
  (let [im 139968
        new-seed (mod (+ (* seed 3877) 29573) im)]
    (pair new-seed (/ (* max seed) im))))


;;
;; constanza!

(def UP 0)
(def RIGHT 1)
(def DOWN 2)
(def LEFT 3)

(def DIRECTIONS (list UP RIGHT DOWN LEFT))

(def WALL 0)
(def EMPTY 1)
(def PILL 2)
(def POWER-PILL 3)
(def FRUIT 4)
(def LM 5)
(def GHOST 6)


;;
;; DSLza!

(defn world-map [world] (head world))
(defn lambda-man [world] (nth world 1))
(defn ghosts [world] (nth world 2))
(defn fruit [world] (nth world 3))
(defn location [actor] (nth actor 1))
(defn direction [actor] (nth actor 2))
(defn at [world-map xy]
  (nth (nth world-map (tail xy)) (head xy)))

(defn neighbour [loc direction]
  (let [x (head loc)
        y (tail loc)]
    (if (= direction UP)
      (pair x (dec y))
      (if (= direction LEFT)
        (pair (dec x) y)
        (if (= direction DOWN)
          (pair x (inc y))
          (pair (inc x) y))))))

(defn next [direction]
  (if (= direction LEFT)
    UP
    (let [pos (index DIRECTIONS direction)]
      (nth DIRECTIONS (inc pos)))))

(defn back [direction]
  (if (= direction UP)
    LEFT
    (let [pos (index DIRECTIONS direction)]
      (nth DIRECTIONS (dec pos)))))


;;
;; logika

(defn main [initial-world ghost-ai]
  (pair 42
        (fn [state world]
          (let [lm (lambda-man world)
                wm (world-map world)
                gs (ghosts world)
                loc (location lm)
                dir (direction lm)
                free? (fn [direction]
                        (not (= WALL (at wm (neighbour loc direction)))))
                free-directions (filter free? DIRECTIONS)
                df (length free-directions)]

            (if (= df 1)
              (pair state (head directions))
              (let [random-data (random state df)
                    next-state (head random-data)
                    random-dir (nth free-directions (tail random-data))]
                (pair next-state random-dir)))))))
