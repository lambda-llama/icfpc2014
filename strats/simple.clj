(defn main [world undocumented]
  (pair 0 step_fn))

(defn step_fn [state world]
  (let [lm (lambda-man world)
        dir (direction lm)
        loc (location lm)
        t1 (next dir)
        t2 (back t1)
        t3 (back t2)
        is-free (fn [t] (not (== WALL (neighbour loc t))))]
    (pair state
          (if (is-free t1)
            t1
            (if (is-free t2)
              t2
              t3)))))
