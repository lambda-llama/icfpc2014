(let [nth (fn [lst pos]
            (if (= pos 0)
              (head lst)
              (nth (tail lst) (dec pos))))]
  (nth (list 1 2 3 4 5) 2))
