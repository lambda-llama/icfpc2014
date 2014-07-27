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

(defn distance [xy1 xy2]
  (let [dx (- (head xy2) (head xy1))
        dy (- (tail xy2) (tail xy1))]
    (+ (* dx dx) (* dy dy))))

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

(defn random-directions [state current-dir free-dirs-locs ghosts]
  (let [free-dirs (map fst free-dirs-locs)
        df (length free-dirs)]
    (if (= df 1)
      (pair state (head free-dirs))
      (let [random-data (random state df)
            next-state (head random-data)
            random-dir (nth free-dirs (tail random-data))]
        (pair next-state random-dir)))))

(defn random-directions-no-back [state current-dir free-dirs-locs ghosts]
  (let [free-dirs-locs-no-back (filter
                                (fn [dl] (not= (fst dl) (back (back current-dir))))
                                free-dirs-locs)
        next-free-dirs-locs (if (empty? free-dirs-locs-no-back)
                         free-dirs-locs
                         free-dirs-locs-no-back)]
    (random-directions state current-dir next-free-dirs-locs ghosts)))

(defn catch-ghosts [state current-dir free-dirs-locs ghosts]
  (let [ghost-locs (map location ghosts)
        dist-fn (fn [dir-loc]
                  (min (map (fn [ghost-loc] (distance (snd dir-loc) ghost-loc)) ghost-locs)))
        best-dir-loc (trace (min-by dist-fn free-dirs-locs))]
    (pair state (fst best-dir-loc))))

(defn step [state world]
  (let [lm (lambda-man world)
        wm (world-map world)
        loc (location lm)
        dirs-locs (map (fn [dir] (pair dir (neighbour loc dir)))
                       DIRECTIONS)
        free-dirs-locs (filter (fn [dl] (free? wm (snd dl))) dirs-locs)]
    (random-directions-no-back state
                               (direction lm)
                               free-dirs-locs
                               (ghosts world))))

(defn main [initial-world ghost-ai]
  (let [initial-state (sum-by (fn [program]
                                (sum-by (fn [op] (head op)) program))
                              ghost-ai)]
    (pair initial-state step)))
