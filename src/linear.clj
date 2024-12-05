(ns linear)

(defn operation [f & vector]
  (apply mapv f vector))

(defn v+ [& vectors]
  (apply operation + vectors))

(defn v- [& vectors]
  (apply operation - vectors))

(defn v* [& vectors]
  (apply operation * vectors))

(defn vd [& vectors]
  (apply operation / vectors))

(defn dot [& vectors]
  (if (empty? vectors) 0
  (reduce + (apply v* vectors))))

(defn v*s [vector & scalars]
  (let [prod (apply * scalars)]
    (operation #(* % prod) vector)))

(defn m+ [& matrices]
  (apply operation v+ matrices))

(defn m- [& matrices]
  (apply operation v- matrices))

(defn m* [& matrices]
  (apply operation v* matrices))

(defn md [& matrices]
  (apply operation vd matrices))

(defn m*s [matrices & scalars]
  (let [prod (apply * scalars)]
    (operation #(operation (fn [param1] (* param1 prod)) %) matrices)))

(defn transpose [matrices]
  (apply operation vector matrices))


(defn m*v [matrices vector]
  (operation #(dot vector %) matrices))

(defn m*m [& matrices]
  (reduce  (fn [A, B] (operation #(m*v (transpose B) %) A)) matrices))