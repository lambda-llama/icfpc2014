(pair 0
      (fn [state world]
        (let [nth (fn [lst pos]
                    (if (= pos 0)
                      (head lst)
                      (nth (tail lst) (dec pos))))
              lookup (fn [lst elem n]
                       (if (= (head lst) elem)
                         n
                         (lookup (tail lst) elem (inc n))))

              UP 0
              LEFT 1
              DOWN 2
              RIGHT 3
              DIRECTIONS (list UP RIGHT DOWN LEFT)
              WALL 0
              EMPTY 1
              PILL 2
              POWER-PILL 3
              FRUIT 4
              LM 5
              GHOST 6
              world-map (fn [world] (head world))
              lambda-man (fn [world] (nth world 2))
              direction (fn [world] (nth world 3))
              location (fn [actor] (nth actor 2))
              at (fn [world-map x y]
                   (nth (nth (world-map) y) x))
              neighbour (fn [pos direction]
                          (let [x (head pos)
                                y (tail pos)]
                            (if (= direction UP)
                              (pair x (dec y))
                              (if (= direction LEFT)
                                (pair (dec x) y)
                                (if (= direction DOWN)
                                  (pair x (inc y))
                                  (pair (inc x) y))))))
              next (fn [direction]
                     (if (= direction LEFT)
                       UP
                       (let [pos (lookup DIRECTIONS direction 0)]
                         (nth DIRECTIONS (inc pos)))))
              back (fn [direction]
                     (if (= direction UP)
                       RIGHT
                       (let [pos (lookup DIRECTIONS direction 0)]
                         (nth DIRECTIONS (dec pos)))))

              lm (lambda-man world)
              dir (direction lm)
              loc (location lm)
              t1 (next dir)
              t2 (back t1)
              t3 (back t2)
              is-free (fn [t] (not (= WALL (neighbour loc t))))]
          (pair state
                (if (is-free t1)
                  t1
                  (if (is-free t2)
                    t2
                    t3))))))
