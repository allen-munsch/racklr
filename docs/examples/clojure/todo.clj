(ns todo)

(def items ["milk" "eggs" "bread"])

(defn first-item [xs]
  (first xs))

(defn describe [xs]
  (if (empty? xs) "none" "some"))

(println (first-item items))
(println (describe items))
