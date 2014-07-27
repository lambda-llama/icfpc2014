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
(defn vitality [actor] (head actor))
(defn location [actor] (nth actor 1))
(defn direction [actor] (nth actor 2))

(defn at [world-map xy]
  (nth (nth world-map (tail xy)) (head xy)))

(defn distance [xy1 xy2]
  (let [dx (- (head xy2) (head xy1))
        dy (- (tail xy2) (tail xy1))]
    (+ (* dx dx) (* dy dy))))

(defn min-distance [base-loc target-locs]
  (min (map (fn [target-loc]
              (distance base-loc target-loc))
            target-locs)))

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

(defn bfs [wm initial-loc target-locs]
  (let [num-cols (length wm)
        num-rows (length (head wm))
        hash (fn [loc] (+ (* (inc (fst loc)) num-rows) (snd loc)))
        target-hashes (map hash target-locs)
        okay? (fn [loc]
                     (let [mark (at wm loc)]
                       (and (not= WALL mark)
                            (not= GHOST mark))))
        adjacent (fn [loc]
                   (let [adjacent-locs (map (fn [dir] (neighbour loc dir))
                                            DIRECTIONS)]
                     (filter okay? adjacent-locs)))
        go (fn [q seen acc depth]
             (if (queue-empty? q)
               acc
               (let [p (queue-pop q)
                     next-pair (fst p)  ;; (depth, loc)
                     next-depth (fst next-pair)
                     next (snd next-pair)
                     next-hash (hash next)]
                 ;; set is integer-only.
                 (if (set-contains? seen next-hash)
                   (go (snd p) seen acc depth)
                   (let [new-seen (set-add seen next-hash)
                         new-q (queue-push-all (snd p)
                                               (map (fn [loc] (pair depth loc))
                                                    (adjacent next)))
                         new-acc (if (contains? target-hashes next-hash)
                                   (let [i (index target-hashes next-hash)]
                                     (pair (pair i next-depth) acc))
                                   acc)]
                     (go new-q new-seen new-acc (inc depth)))))))]
    (let [q (queue-push (queue) (pair 0 initial-loc))]
      (go q (set) 0 0))))

;;
;; logika

(defn random-directions [wm state
                         current-loc current-dir
                         free-dirs-locs ghosts]
  (let [free-dirs (map fst free-dirs-locs)
        df (length free-dirs)]
    (if (= df 1)
      (pair state (head free-dirs))
      (let [random-data (random state df)
            next-state (head random-data)
            random-dir (nth free-dirs (tail random-data))]
        (pair next-state random-dir)))))

(defn random-directions-no-back [wm state
                                 current-loc current-dir
                                 free-dirs-locs ghosts]
  (let [free-dirs-locs-no-back (filter
                                (fn [dl] (not= (fst dl) (back (back current-dir))))
                                free-dirs-locs)
        next-free-dirs-locs (if (empty? free-dirs-locs-no-back)
                              free-dirs-locs
                              free-dirs-locs-no-back)]
    (random-directions wm state
                       current-loc current-dir
                       next-free-dirs-locs ghosts)))

(defn catch-or-avoid-ghosts [wm state
                             current-loc current-dir
                             free-dirs-locs ghosts
                             avoid?]
  (let [ghost-locs (map location ghosts)
        best-dir-loc (min-by (fn [dir-loc]
                               (let [d (min-distance (snd dir-loc) ghost-locs)]
                                 (if avoid?
                                   (neg d)
                                   d)))
                             free-dirs-locs)]
    (pair state (fst best-dir-loc))))

(defn random-or-runaway [wm state
                         current-loc current-dir
                         free-dirs-locs ghosts]
  (let [ghost-locs (map location ghosts)
        ghost-distance (min-distance current-loc ghost-locs)]
    (if (< ghost-distance 3)
      (catch-or-avoid-ghosts
       wm state current-loc current-dir free-dirs-locs ghosts 1)
      (random-directions-no-back
       wm state current-loc current-dir free-dirs-locs ghosts))))

(defn step [state world]
  (let [lm (lambda-man world)
        wm (world-map world)
        loc (location lm)
        dirs-locs (map (fn [dir] (pair dir (neighbour loc dir)))
                       DIRECTIONS)
        free-dirs-locs (filter (fn [dl] (free? wm (snd dl))) dirs-locs)]
    (if (vitality lm)
      (catch-or-avoid-ghosts wm state loc (direction lm)
                             free-dirs-locs
                             (ghosts world)
                             0)
          (random-or-runaway wm state
                         loc (direction lm)
                         free-dirs-locs
                         (ghosts world)))))

(defn main [initial-world ghost-ai]
  (let [initial-state (sum-by (fn [program]
                                (sum-by (fn [op] (head op)) program))
                              ghost-ai)]
    (pair initial-state step)))
