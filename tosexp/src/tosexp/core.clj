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
  (if (seq args)
    (let [[k v] (take 2 args)]
      (list 'Letrec (list (list (transform k) (transform v)))
            (let-macro (drop 2 args) body)))
    (transform body)))

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

(defn transform-reducer [acc form]
  (match
   [(if (list? form) (vec form) form)]

   [['defn 'main (args :guard #(and (vector? %)
                                    (= 2 (count %)))) body]]
   (assoc acc :main
              {:args args
               :body body})

   [['defn name (args :guard vector?) body]]
   (update-in acc [:forms]
              conj {:type :defn
                    :name name
                    :args args
                    :body body})

   [['def name body]]
   (update-in acc [:forms]
              conj {:type :def
                    :name name
                    :body body})))

(defn transform-to-let [acc]
  (if (seq (:forms acc))
    (let [form (first (:forms acc))]
      (list 'let [(:name form)
                  (case (:type form)
                    :def (:body form)
                    :defn (list 'fn (:args form) (:body form)))]
            (transform-to-let (update-in acc [:forms] rest))))
    (-> acc :main :body)))

(defn process-form-seq [form-seq]
  (->> form-seq
       (reduce transform-reducer {:forms []})
       transform-to-let
       transform))

(defn -main
  [& args]
  (->> (repeatedly #(read *in* false :end))
       (take-while (complement (partial = :end)))
       process-form-seq
       pp/pprint))
