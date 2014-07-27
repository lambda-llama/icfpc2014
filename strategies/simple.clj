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

(defn random-directions [state free-directions]
  (let [df (length free-directions)]
    (if (= df 1)
      (pair state (head free-directions))
      (let [random-data (random state df)
            next-state (head random-data)
            random-direction (nth free-directions (tail random-data))]
        (pair next-state random-direction)))))

(defn step [state world]
  (let [lm (lambda-man world)
        wm (world-map world)
        loc (location lm)
        free? (fn [direction]
                (not (= WALL (at wm (neighbour loc direction)))))
        free-directions (filter free? DIRECTIONS)]
    (random-directions state
                       free-directions)))

(defn main [initial-world ghost-ai]
  (pair 42 step))
