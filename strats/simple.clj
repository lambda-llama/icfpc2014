(defn main [world undocumented]
  '(initial_state step_fn))

(def initial_state
  0)

(defn step_fn [state world]
  (let [lm (lambda-man world)
        dir (direction lm)
        loc (location lm)
        t1 (next dir)
        t2 (back t1)
        t3 (back t2)
        is-free (fn [t] (not (== WALL (neighbour loc t))))]
    '(state, (if (is-free t1)
               t1
               (if (is-free t2)
                 t2
                 t3)))))
