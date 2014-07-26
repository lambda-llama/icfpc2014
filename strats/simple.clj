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

(defn world-map [world] (head world))
(defn lambda-man [world] (nth world 1))
(defn location [actor] (nth actor 1))
(defn direction [actor] (nth actor 2))
(defn at [world-map xy]
  (nth (nth (world-map) (head xy)) (tail xy)))

(defn neighbour [pos direction]
  (let [x (head pos)
        y (tail pos)]
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
    (let [pos (lookup DIRECTIONS direction)]
      (nth DIRECTIONS (inc pos)))))

(defn back [direction]
  (if (= direction UP)
    LEFT
    (let [pos (lookup DIRECTIONS direction)]
      (nth DIRECTIONS (dec pos)))))

(defn trace [x] (debug x x))

(defn main [initial-world ghost-ai]
  (pair 0
        (fn [state world]
          (let [lm (lambda-man world)
                wm (world-map world)
                loc (location lm)
                dir (direction lm)
                t1 (next dir)
                t2 (back t1)
                t3 (back t2)
                is-free (fn [t] (not (= WALL (at wm (neighbour loc t)))))]
            (pair state
                  (if (is-free t1)
                    t1
                    (if (is-free t2)
                      t2
                      t3)))))))
