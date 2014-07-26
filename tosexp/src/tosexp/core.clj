(ns tosexp.core
  (:use [clojure.core.match :only (match)])
  (:require [clojure.walk :as w]))

(defn transform [form]
  (match
   [(if (list? form) (vec form) form)]
   [['+ a b]] (list 'Add (transform a) (transform b))
   [(a :guard #(number? %))] (list 'Const a)
   ))

(defn mytest []
  (let []
    '(+ 1 2)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
