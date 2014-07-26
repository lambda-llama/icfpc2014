(ns tosexp.core
  (:use [clojure.core.match :only (match)])
  (:require [clojure.walk :as w])
  (:gen-class))

(declare transform)

(def un-ops
  {'head 'Car
   'tail 'Cdr})

(def bin-ops
  {'+ 'Add
   '- 'Sub
   '* 'Mul
   '/ 'Div
   '= 'Eq
   '< 'Gt
   '<= 'Gte
   '> [:inverted 'Gt]
   '>= [:inverted 'Gte]
   'pair 'Cons})

(defn list-macro [arg & args]
  (if (seq args)
    (list 'Cons (transform arg) (apply list-macro args))
    (list (transform arg) (transform 0))))

(def macroses
  {'list list-macro
   'inc (fn [a] (transform (list '+ a 1)))
   'dec (fn [a] (transform (list '- a 1)))})

(defn transform [form]
  (match
   [(if (list? form) (vec form) form)]

   [[(op :guard un-ops) a]]
   (list (un-ops op) a)

   [[(op :guard bin-ops) a b]]
   (if (symbol? (bin-ops op))
     (list (bin-ops op) (transform a) (transform b))
     (if (and (vector? (bin-ops op))
              (= :inverted (first (bin-ops op))))
       (list (second (bin-ops op)) (transform b) (transform a))
       (throw "bad binop")))

   [['fn (args :guard vector?) body]]
   (list 'Fn (apply list args) (transform body))

   [['if guard t f]]
   (list 'If (transform guard) (transform t) (transform f))

   [[(macros :guard macroses) & args]]
   (apply (macroses macros) args)

   [(a :guard number?)]
   (list 'Const a)

   [(a :guard symbol?)]
   (list 'Var a)))

(defn -main
  [& args]
  (-> (slurp (java.io.BufferedReader. *in*))
      read-string
      transform
      println))
