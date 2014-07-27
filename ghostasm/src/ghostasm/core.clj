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

(declare int-by-name)

(defn call-by-label [lbl]
  [(MOV :h :pc) ;; return address in H register
   (jmp (label lbl))])

(defn return [] ;; g and h are used
  [(MOV :g :h)
   (MOV :h (label :crash))
   (ADD :g 2)
   (MOV :pc :g)])

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

(def mtx1 {:offset 30
           :nrows 5
           :ncols 5})

(def mtx2 {:offset 100
           :nrows 5
           :ncols 5})

(defn matrix-read [mtx i j] ;; leaves value at a
  [(MOV :a i)
   (MUL :a (:nrows mtx))
   (ADD :a j)
   (ADD :a (:offset mtx))
   (MOV :a (at :a))])

(defn matrix-write [mtx i j value]
  [(MOV :a i)
   (MUL :a (:nrows mtx))
   (ADD :a j)
   (ADD :a (:offset mtx))
   (MOV (at :a) value)])

(defn mytest []
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
     (int-by-name :first-lambda)
     (MOV :c :a)
     (MOV :d :b)
     (int-by-name :my-index)
     (int-by-name :current-pos)
     (MOV :e :a)
     (MOV :f :b)
     ;; left
     (DEC :e)
     (call-by-label :find-h)
     (MOV (at 3) :a)
     ;; right
     (ADD :e 2)
     (call-by-label :find-h)
     (MOV (at 1) :a)
     ;; up
     (DEC :e)
     (DEC :f)
     (call-by-label :find-h)
     (MOV (at 0) :a)
     ;; down
     (ADD :f 2)
     (call-by-label :find-h)
     (MOV (at 2) :a)
     (int-by-name :my-index)
     (int-by-name :current-pos)
     (MOV :e :a)
     (MOV :f :b)
     (MOV :a (at 0))
     (MOV :b (at 1))
     (MOV :c (at 2))
     (MOV :d (at 3))
     (int-by-name :dbg)
     (HLT)

     (label :crash)
     (MOV :a 42)
     (MOV :b 42)
     (MOV :c 42)
     (int-by-name :dbg)
     (HLT)

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

     (label :move-to-min) ;; uses a-c
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
     (INT 0)
     (return)

     (label :zero-out-directions)
     (MOV (at 0) 0)
     (MOV (at 1) 0)
     (MOV (at 2) 0)
     (MOV (at 3) 0)
     (return)

     (label :find-lambdaman) ;; update first four adresses, uses a-d
     (int-by-name :my-index) ;; index in a
     (int-by-name :current-pos) ;; x in a, y in b
     (MOV :c :a)
     (MOV :d :b) ;; a and b will be needed, x now in c, y in d
     (int-by-name :first-lambda) ;; x_lambda in a, y_lambda in b
     (JGT (label :skip-increase-left) :a :c)  ;; if x_lambda > x, don't go left
     (INC (at 3))
     (label :skip-increase-left)
     (JLT (label :skip-increase-down) :b :d) ;; if y_lambda < y, don't go down
     (INC (at 2))
     (label :skip-increase-down)
     (JLT (label :skip-increase-right) :a :c) ;; if x_lambda < x, don't go right
     (INC (at 1))
     (label :skip-increase-right)
     (JGT (label :skip-increase-up) :b :d) ;; if y_lambda > y, don't go right
     (INC (at 0))
     (label :skip-increase-up)
     (XOR (at 0) 1)
     (XOR (at 1) 1)
     (XOR (at 2) 1)
     (XOR (at 3) 1)
     (return)

     (label :add-random) ;; uses a-c
     (MOV :c (at 254))   ;; random seed
     (ADD :c 3)
     (int-by-name :my-index) ;; index in a
     (ADD :c :a)
     (int-by-name :current-pos)
     (ADD :c :a)
     (int-by-name :first-lambda)
     (ADD :c :a)
     (AND :c 3)
     (MOV (at 254) :c)
     (INC (at :c))
     (return)


     ))))


(defn -main
  [& args]
  (prn "hello world"))
