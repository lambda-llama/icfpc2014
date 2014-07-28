(ns ghostasm.core
  (:use [clojure.core.match :only (match)])
  (:require [clojure.walk :as w])
  (:require [clojure.pprint :as pp])
  (:require [clojure.string :as s])
  (:require [plumbing.core :as pl])
  (:gen-class))

(defn MOV [dest src]
  {:type :op
   :op "mov"
   :args [dest src]})

(defn INC [dest]
  {:type :op
   :op "inc"
   :args [dest]})

(defn DEC [dest]
  {:type :op
   :op "dec"
   :args [dest]})

(defn ADD [dest src]
  {:type :op
   :op "add"
   :args [dest src]})

(defn SUB [dest src]
  {:type :op
   :op "sub"
   :args [dest src]})

(defn MUL [dest src]
  {:type :op
   :op "mul"
   :args [dest src]})

(defn DIV [dest src]
  {:type :op
   :op "div"
   :args [dest src]})

(defn AND [dest src]
  {:type :op
   :op "and"
   :args [dest src]})

(defn OR [dest src]
  {:type :op
   :op "or"
   :args [dest src]})

(defn XOR [dest src]
  {:type :op
   :op "xor"
   :args [dest src]})

(defn JLT [targ x y]
  {:type :op
   :op "jlt"
   :args [targ x y]})

(defn JEQ [targ x y]
  {:type :op
   :op "jeq"
   :args [targ x y]})

(defn JGT [targ x y]
  {:type :op
   :op "jgt"
   :args [targ x y]})

(defn INT [i]
  {:type :op
   :op "int"
   :args [i]})

(defn HLT []
  {:type :op
   :op "hlt"
   :args []})

(defn label [name]
  {:type :label
   :name name})

(defn at [addr]
  {:type :at
   :addr addr})

(defn asm [& insts]
  (vec insts))

(defn format-arg [arg]
  (cond
   (keyword? arg) (name arg)
   (string? arg) arg
   (number? arg) (str arg)
   (map? arg) (if (= :at (:type arg))
                (str "[" (format-arg (:addr arg)) "]"))))

(defn formatter [op]
  (->> (map format-arg (:args op))
       (s/join ",")
       (vector (:op op))
       (filter (complement s/blank?))
       (s/join " ")))

(defn dbg [x] (prn x) x)

(defn label-collect-phase [forms]
  (let [reducer
        (fn [acc form]
          (case (:type form)
            :label (update-in acc [:labels]
                              assoc (:name form) (:counter acc))
            :op {:labels (:labels acc)
                 :forms (conj (:forms acc) form)
                 :counter (inc (:counter acc))}))]
    (reduce reducer
            {:labels {}
             :forms []
             :counter 0}
            forms)))

(defn label-replace-phase [{:keys [labels forms]}]
  (for [form forms]
    (assoc form
      :args (for [arg (:args form)]
              (if (and (map? arg)
                       (= (:type arg) :label))
                (pl/safe-get labels (:name arg)) ;; SAFE-GET HERE
                arg)))))

(defn line-enumerate-phase [lines]
  (let [max-len (->> lines (map count) (reduce max 0))
        target-len (+ max-len 2)]
    (map-indexed
     (fn [idx line]
       (str line (apply str (repeat (- target-len (count line)) " "))
           ";; " idx))
     lines)))

(defn asm-compile [insts]
  (let [code-by-str (->> insts
                         flatten
                         label-collect-phase
                         label-replace-phase
                         (map formatter)
                         line-enumerate-phase)
        code (s/join "\n" code-by-str)]
    (println (str "len: " (count code-by-str) "/256\n" ))
    code))

(defn jmp [addr]
  (JEQ addr 0 0))

(def ACCUM-OFFSET 20) ;; first item is current accum head pointer
(def ACCUM-EL-LENGTH 2)  ;; 2 bytes per item (x, y)
(def ACCUM-LENGTH 30)
(def VISITED-PENALTY 20)

(declare int-by-name)

(defn init []
  [(MOV :h 255)
   (JGT (label :skip-accum-init) (at ACCUM-OFFSET) 0)
   (MOV (at ACCUM-OFFSET) (+ ACCUM-OFFSET 1))
   (label :skip-accum-init)])

(defn call-by-label [lbl]
  [(DEC :h)  ;; h points on top of call stack
   (MOV (at :h) :pc)
   (ADD (at :h) 3) ;; return address should point on the next instruction
   (jmp (label lbl))])

(defn return [] ;; g and h are used
  [(MOV :g :h)
   (INC :h)
   (JLT (label :crash) :h :g) ;; check if h overflowed
   (MOV :pc (at :g))])

(defn int-by-name [name]
  (INT (pl/safe-get {:direction 0
                     :first-lambda 1
                     :second-lambda 2
                     :my-index 3
                     :starting-pos 4
                     :current-pos 5
                     :ghost-info 6
                     :map 7
                     :dbg 8}
                    name)))

(defn genasm []
  (println
   (asm-compile
    (asm
     ;; (INC (at :b)) ;; change this
     ;; (call-by-label :zero-out-directions)
     ;; (call-by-label :find-lambdaman)
     ;; #_(call-by-label :add-random)
     ;; (call-by-label :move-to-min)
     ;; (MOV :a (at 0))
     ;; (MOV :b (at 1))
     ;; (MOV :c (at 2))
     ;; (MOV :d (at 3))
     ;; 0 1 2 3 : directions
     (init)

     (call-by-label :find-hs-around)
     (call-by-label :is-on-crossing)
     (JEQ (label :no-crossing-yet) :a 0)
     ;; we are on crossing! time to decide where to move
     (call-by-label :is-frightened-set)
     (call-by-label :is-visited)
     (call-by-label :find-min)
     (INT 0)
     (MOV :c :a)
     (MOV :a 4)
     (MOV :b 8)
     (ADD :a :c)
     (ADD :b :c)
     (MOV :a (at :a))
     (MOV :b (at :b))
     (call-by-label :save-coords)
     ;; (MOV :a (at 0))
     ;; (MOV :b (at 1))
     ;; (MOV :c (at 2))
     ;; (MOV :d (at 3))
     ;; #_(int-by-name :dbg)
     ;; (MOV :a (at 4))
     ;; (MOV :b (at 5))
     ;; (MOV :c (at 6))
     ;; (MOV :d (at 7))
     ;; #_(int-by-name :dbg)
     ;; (MOV :a (at 8))
     ;; (MOV :b (at 9))
     ;; (MOV :c (at 10))
     ;; (MOV :d (at 11))
     ;; #_(int-by-name :dbg)
     ;; (MOV :a (at ACCUM-OFFSET))
     ;; (MOV :b (at (+ ACCUM-OFFSET 1)))
     ;; (MOV :c (at (+ ACCUM-OFFSET 2)))
     ;; (MOV :d (at (+ ACCUM-OFFSET 3)))
     ;; (MOV :e (at (+ ACCUM-OFFSET 4)))
     ;; (int-by-name :dbg)
     ;; (MOV :a (at (+ ACCUM-OFFSET 5)))
     ;; (MOV :b (at (+ ACCUM-OFFSET 6)))
     ;; (MOV :c (at (+ ACCUM-OFFSET 7)))
     ;; (MOV :d (at (+ ACCUM-OFFSET 8)))
     ;; (MOV :e (at (+ ACCUM-OFFSET 9)))
     ;; (int-by-name :dbg)
     (int-by-name :my-index)
     (int-by-name :current-pos)
     (call-by-label :save-coords)
     (label :no-crossing-yet)
     (HLT)

     (label :crash)
     (MOV :a 42)
     (MOV :b 42)
     (MOV :c 42)
     (int-by-name :dbg)
     (HLT)

     (label :is-frightened-set)
     (int-by-name :my-index)
     (int-by-name :ghost-info)
     (JEQ (label :is-frightened-true) :a 1)
     (jmp (label :is-frightened-false))

     (label :is-frightened-true)
     (JEQ (label :is-frightened-skip-0) (at 0) 255)
     (XOR (at 0) 255)
     (label :is-frightened-skip-0)
     (JEQ (label :is-frightened-skip-1) (at 1) 255)
     (XOR (at 1) 255)
     (label :is-frightened-skip-1)
     (JEQ (label :is-frightened-skip-2) (at 2) 255)
     (XOR (at 2) 255)
     (label :is-frightened-skip-2)
     (JEQ (label :is-frightened-skip-3) (at 3) 255)
     (XOR (at 3) 255)
     (label :is-frightened-skip-3)
     (label :is-frightened-false)
     (return)


     (label :is-visited) ;; assumes that xs of nearby cells are in 4..7
                         ;; ys are in 8..11
                         ;; adds penalty to 0..4 if direction was visited
                         ;; uses a-e
     (MOV :a 0) ;; direction

     (label :is-visited-search-loop-init)
     (MOV :b 0) ;; if this direction was visited, this will be 1
     (MOV :c 4) ;; index of x of current direction
     (MOV :d 8) ;; indef of y of current direction
     (ADD :c :a)
     (ADD :d :a)
     (MOV :e (+ ACCUM-OFFSET 1)) ;; pointer into array

     (label :is-visited-search-loop-start)
     (JEQ (label :is-visited-compare-ys) (at :c) (at :e))
     (jmp (label :is-visited-search-loop-cont))

     (label :is-visited-compare-ys)
     (INC :e)
     (JEQ (label :is-visited-found) (at :d) (at :e))
     (DEC :e)
     (jmp (label :is-visited-search-loop-cont))

     (label :is-visited-found)
     (MOV :b 1)
     (jmp (label :is-visited-search-loop-stop))

     (label :is-visited-search-loop-cont)
     (ADD :e ACCUM-EL-LENGTH)
     (JLT (label :is-visited-search-loop-start)
          :e (+ ACCUM-OFFSET 1 ;; array starts here
                (* ACCUM-EL-LENGTH ACCUM-LENGTH)
                1))

     (label :is-visited-search-loop-stop)
     (JEQ (label :direction-loop-cont) :b 0)
     (ADD (at :a) VISITED-PENALTY)

     (label :direction-loop-cont)
     (INC :a)
     (JLT (label :is-visited-search-loop-init) :a 4)
     (return)



     (label :save-coords) ;; assumes coordinates that need to be saved are
                          ;; in a and b; uses c
     (MOV :c (at ACCUM-OFFSET))
     (MOV (at :c) :a)
     (INC :c)
     (MOV (at :c) :b)
     (ADD (at ACCUM-OFFSET) ACCUM-EL-LENGTH)
     (JLT (label :save-coords-pointer-ok)
          (at ACCUM-OFFSET) (+ ACCUM-OFFSET 1 ;; array starts here
                               (* ACCUM-EL-LENGTH ACCUM-LENGTH)))
     (MOV (at ACCUM-OFFSET) (+ ACCUM-OFFSET 1))
     (label :save-coords-pointer-ok)
     (return)

     (label :is-on-crossing) ;; assumes hs are in 0..4, uses a and b;
                             ;; leaves 0/1 in a (0 if it's not a crossing yet)
     (MOV :a 0) ;; idx
     (MOV :b 0) ;; counter (how many walls are around)
     (label :is-on-crossing-loop-start)
     (JLT (label :is-on-crossing-no-inc) (at :a) 255)
     (INC :b)
     (label :is-on-crossing-no-inc)
     (INC :a)
     (JLT (label :is-on-crossing-loop-start) :a 4)
     (MOV :a 0)
     (JGT (label :is-on-crossing-false) :b 1)
     (MOV :a 1)
     (label :is-on-crossing-false)
     (return)


     (label :find-hs-around) ;; doesn't assume anything, leaves hs in 0..3,
                             ;; xs of nearby cells in 4..7
                             ;; ys of nearby cells in 8..11
     (int-by-name :first-lambda)
     (MOV :c :a)
     (MOV :d :b)
     (int-by-name :my-index)
     (int-by-name :current-pos)
     (MOV :e :a)  ;; current x and y are now in e and f
     (MOV :f :b)
     ;; left
     (DEC :e)
     (MOV (at 7) :e)
     (MOV (at 11) :f)
     (call-by-label :find-h)
     (MOV (at 3) :a)
     ;; right
     (ADD :e 2)
     (MOV (at 5) :e)
     (MOV (at 9) :f)
     (call-by-label :find-h)
     (MOV (at 1) :a)
     ;; up
     (DEC :e)
     (DEC :f)
     (MOV (at 4) :e)
     (MOV (at 8) :f)
     (call-by-label :find-h)
     (MOV (at 0) :a)
     ;; down
     (ADD :f 2)
     (MOV (at 6) :e)
     (MOV (at 10) :f)
     (call-by-label :find-h)
     (MOV (at 2) :a)
     (return)


     (label :find-h) ;; assumes c and d are lambda_coords, e and f are coords
                     ;; uses a-b; returns heuristic in a
     (MOV :a :e)
     (MOV :b :f)
     (int-by-name :map)
     (JGT (label :find-h-nowall) :a 0)
     (MOV :a 255)
     (return)
     (label :find-h-nowall)
     (JGT (label :find-h-x-larger) :e :c)
     (MOV :a :c)
     (SUB :a :e)
     (jmp (label :find-h-x-done))
     (label :find-h-x-larger)
     (MOV :a :e)
     (SUB :a :c)
     (label :find-h-x-done)
     (JGT (label :find-h-y-larger) :f :d)
     (MOV :b :d)
     (SUB :b :f)
     (jmp (label :find-h-y-done))
     (label :find-h-y-larger)
     (MOV :b :f)
     (SUB :b :d)
     (label :find-h-y-done)
     (ADD :a :b)
     (return)


     (label :find-min) ;; uses a-c; leaves min direction in a
     (MOV :a 255)
     (MOV :b 0)
     (MOV :c 255)
     (label :min-loop-start)
     (INC :c)
     (JGT (label :min-loop-end) (at :c) :a)
     (MOV :a (at :c))
     (MOV :b :c)
     (label :min-loop-end)
     (JLT (label :min-loop-start) :c 3)
     (MOV :a :b)
     (return)


     (label :zero-out-directions)
     (MOV (at 0) 0)
     (MOV (at 1) 0)
     (MOV (at 2) 0)
     (MOV (at 3) 0)
     (return)


     ;; (label :find-lambdaman) ;; update first four adresses, uses a-d
     ;; (int-by-name :my-index) ;; index in a
     ;; (int-by-name :current-pos) ;; x in a, y in b
     ;; (MOV :c :a)
     ;; (MOV :d :b) ;; a and b will be needed, x now in c, y in d
     ;; (int-by-name :first-lambda) ;; x_lambda in a, y_lambda in b
     ;; (JGT (label :skip-increase-left) :a :c)  ;; if x_lambda > x, don't go left
     ;; (INC (at 3))
     ;; (label :skip-increase-left)
     ;; (JLT (label :skip-increase-down) :b :d) ;; if y_lambda < y, don't go down
     ;; (INC (at 2))
     ;; (label :skip-increase-down)
     ;; (JLT (label :skip-increase-right) :a :c) ;; if x_lambda < x, don't go right
     ;; (INC (at 1))
     ;; (label :skip-increase-right)
     ;; (JGT (label :skip-increase-up) :b :d) ;; if y_lambda > y, don't go right
     ;; (INC (at 0))
     ;; (label :skip-increase-up)
     ;; (XOR (at 0) 1)
     ;; (XOR (at 1) 1)
     ;; (XOR (at 2) 1)
     ;; (XOR (at 3) 1)
     ;; (return)


     ;; (label :add-random) ;; uses a-c
     ;; (MOV :c (at 254))   ;; random seed
     ;; (ADD :c 3)
     ;; (int-by-name :my-index) ;; index in a
     ;; (ADD :c :a)
     ;; (int-by-name :current-pos)
     ;; (ADD :c :a)
     ;; (int-by-name :first-lambda)
     ;; (ADD :c :a)
     ;; (AND :c 3)
     ;; (MOV (at 254) :c)
     ;; (INC (at :c))
     ;; (return)



     ))))


(defn -main
  [& args]
  (prn "hello world"))
