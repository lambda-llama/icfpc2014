(def UP 0)
(def LEFT 1)
(def DOWN 2)
(def RIGHT 3)
(def DIRECTIONS [UP RIGTH DOWN LEFT UP RIGHT])


(def WALL 0)
(def EMPTY 1)
(def PILL 2)
(def POWER-PILL 3)
(def FRUIT 4)
(def LM 5)
(def GHOST 6)


(defn world-map [world]
  (head world))

(defn lambda-man [world]
  (nth world 2))

(defn direction [actor]
  (nth actor 3))

(defn location [actor]
  (nth actor 2))

(defn at [world-map x y]
  (nth (nth (world-map) y) x))

(defn neighbour [pos direction]
  (let [x (head pos)
        y (tail pos)]
    (if (== direction UP)
      '(x, (prev y))
      (if (== direction LEFT)
        '((prev x), y)
        (if (== direction DOWN)
          '(x, (succ y))
          '((succ x), y))))))

(defn next [direction]
  (nth DIRECTIONS (succ direction)))

(defn back [direction]
  (if (== direction UP)
    RIGHT
    (nth DIRECTIONS (pred direction))))
