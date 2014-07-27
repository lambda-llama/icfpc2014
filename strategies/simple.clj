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

(defn free? [wm loc]
  (not= WALL (at wm loc)))

;;
;; logika

(defn random-directions [state free-dirs]
  (let [df (length free-dirs)]
    (if (= df 1)
      (pair state (head free-dirs))
      (let [random-data (random state df)
            next-state (head random-data)
            random-dir (nth free-dirs (tail random-data))]
        (pair next-state random-dir)))))

(defn ghost-runaway [state current-dir free-dirs ghosts]
  (let [ghost-locs (map location ghosts)]
    (pair state current-dir)))

(defn step [state world]
  (let [lm (lambda-man world)
        wm (world-map world)
        loc (location lm)
        free-dirs (filter (fn [dir]
                            (free? wm (neighbour loc dir)))
                          DIRECTIONS)]
    (random-directions state
                       free-dirs)))

(def initial-state
  42)

(defn main [initial-world ghost-ai]
  (pair initial-state step))
