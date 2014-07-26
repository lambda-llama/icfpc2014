(let [initial-state 0
      left (pair 0 1)
      right (pair 1 3)]
  (let [step-fn (fn [state]
                (if state left right))]
    (pair initial-state step-fn)))
