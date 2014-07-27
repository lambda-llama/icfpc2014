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

(defn asm-compile [insts]
  (let [code-by-str (->> insts
                         flatten
                         label-collect-phase
                         label-replace-phase
                         (map formatter))
        code (s/join "\n" code-by-str)]
    (println (str "len: " (count code-by-str) "/256\n" ))
    code))

(defn jmp [addr]
  (JEQ addr 0 0))

(declare int-by-name)

(defn call-by-label [lbl]
  [(MOV :h :pc) ;; return address in H register
   (jmp (label lbl))])

(defn return []
  (MOV :pc :h))

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

(defn mytest []
  (println
   (asm-compile
    (asm
     (MOV :a 0)
     (MOV :b 0)
     (MOV :c 255)

     (label :min-loop-start)
     (INC :c)
     (JLT (label :min-loop-end) (at :c) :a)
     (MOV :a (at :c))
     (MOV :b :c)
     (label :min-loop-end)
     (int-by-name :dbg)
     (JLT (label :min-loop-start) :c 3)

     (MOV :a :b)
     (INT 0)
     (INT 3)
     (INT 6)
     ;; (INC (at :b)) ;; change this
     (call-by-label :find-lambdaman)
     (HLT)

     (label :find-lambdaman) ;; update first four adresses, uses a-d
     (MOV (at 0) 0)
     (MOV (at 1) 0)
     (MOV (at 2) 0)
     (MOV (at 3) 0)
     (int-by-name :my-index) ;; index in a
     (int-by-name :current-pos) ;; x in a, y in b
     (MOV :c :a)
     (MOV :d :b) ;; a and b will be needed, x now in c, y in d
     (int-by-name :first-lambda) ;; x_lambda in a, y_lambda in b
     (JLT (label :skip-increase-left) :a :c)  ;; if x_lambda < x, don't go left
     (INC (at 3))
     (label :skip-increase-left)
     (JLT (label :skip-increase-down) :b :d) ;; if y_lambda < y, don't go down
     (INC (at 2))
     (label :skip-increase-down)
     (JGT (label :skip-increase-right) :a :c) ;; if x_lambda > x, don't go right
     (INC (at 1))
     (label :skip-increase-right)
     (JGT (label :skip-increase-up) :b :d) ;; if y_lambda > y, don't go right
     (INC (at 0))
     (label :skip-increase-up)
     (MOV :d (at 0))
     (MOV :e (at 1))
     (MOV :f (at 2))
     (MOV :g (at 3))
     #_(int-by-name :dbg)
     (return)))))


(defn -main
  [& args]
  (prn "hello world"))
