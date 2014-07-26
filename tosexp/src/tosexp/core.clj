(ns tosexp.core
  (:use [clojure.core.match :only (match)])
  (:require [clojure.walk :as w])
  (:require [clojure.pprint :as pp])
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
    (list 'Cons (transform arg) (transform 0))))

(defn let-macro [args body]
  (let [args (partition 2 args)
        ks (map first args)
        vs (map second args)]
    (list 'Call
          (list 'Fn (apply list ks) (transform body))
          (apply list (map transform vs)))))

(def macroses
  {'list list-macro
   'not (fn [arg] (transform (list '- 1 arg)))
   'let let-macro
   'inc (fn [a] (transform (list '+ a 1)))
   'dec (fn [a] (transform (list '- a 1)))})

(defn transform [form]
  (match
   [(if (list? form) (vec form) form)]

   [[(op :guard un-ops) a]]
   (list (un-ops op) (transform a))

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

   [[a & args]]
   (list 'Call (transform a) (apply list (map transform args)))

   [(a :guard number?)]
   (list 'Const a)

   [(a :guard symbol?)]
   (list 'Var a)))

(defn -main
  [& args]
  (-> (slurp (java.io.BufferedReader. *in*))
      read-string
      transform
      pp/pprint))
