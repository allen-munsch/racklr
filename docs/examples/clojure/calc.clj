(ns calc)

(defn add [a b]
  (+ a b))

(defn square [x]
  (* x x))

(def total (add 2 3))

(println (square total))
