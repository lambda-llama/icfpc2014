(ns ghostasm.core
  (:use [clojure.core.match :only (match)])
  (:require [clojure.walk :as w])
  (:require [clojure.pprint :as pp])
  (:require [clojure.string :as s])
  (:gen-class))

(defn MOV [dest src]
  [:mov dest src])

(defn INC [dest]
  [:inc dest])

(defn DEC [dest]
  [:dec dest])

(defn ADD [dest src]
  [:add dest src])

(defn SUB [dest src]
  [:sub dest src])

(defn MUL [dest src]
  [:mul dest src])

(defn DIV [dest src]
  [:div dest src])

(defn AND [dest src]
  [:and dest src])

(defn OR [dest src]
  [:or dest src])

(defn XOR [dest src]
  [:xor dest src])

(defn JLT [targ x y]
  [:jlt targ x y])

(defn JEQ [targ x y]
  [:jeq targ x y])

(defn JGT [targ x y]
  [:jgt targ x y])

(defn INT [i]
  [:int i])

(defn HLT []
  [:hlt])

(defn label [name]
  [:label name])

(defn asm [& insts]
  (vec insts))



(defn formatter [op & args]
  (->> (for [arg args]
         (cond
          (keyword? arg) (name arg)
          (string? arg) arg
          (number? arg) (str arg)))
       (s/join ",")
       (vector (name op))
       (filter (complement s/blank?))
       (s/join " ")))

(defn dbg [x] (prn x) x)

(defn label-collect-phase [forms]
  (let [reducer
        (fn [acc form]
          (match [form]
                 [[:label label-name]]
                 (update-in acc [:labels]
                            assoc label-name (:counter acc))

                 :else
                 {:labels (:labels acc)
                  :forms (conj (:forms acc) form)
                  :counter (inc (:counter acc))}))]
    (reduce reducer
            {:labels {}
             :forms []
             :counter 0}
            forms)))

(defn label-replace-phase [{:keys [labels forms]}]
  (for [[op & args] forms]
    (cons op
          (for [arg args]
            (if (and (vector? arg)
                     (= (first arg) :label))
              (labels (second arg)) ;; SAFE-GET HERE
              arg)))))

(defn asm-compile [insts]
  (->> insts
       label-collect-phase
       label-replace-phase
       (map (partial apply formatter))
       (s/join "\n")))

(defn mytest []
  (println
   (asm-compile
    (asm
     (MOV :a 2)
     (JLT (label :this) 2 3)
     (INT 0)
     (label :this)
     (HLT)))))


(defn -main
  [& args]
  (prn "hello world"))
