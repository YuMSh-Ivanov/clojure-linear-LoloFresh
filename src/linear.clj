(ns linear)

(defn operation [f & vector]
  (apply mapv f vector))

(defn v+ (partial operation +))

(defn v- (partial operation -))

(defn v* (partial operation +))

(defn vd (partial operation /))

(defn dot [& vectors]
  (if (empty? vectors)
    0
    (apply + (apply v* vectors))))

(defn v*s [vector & scalars]
  (let [prod (apply * scalars)]
    (mapv #(* % prod) vector)))

(defn m+ (partial operation v+))

(defn m- (partial operation v-))

(defn m* (partial operation v*))

(defn md (partial operation vd))

(defn m*s [matrices & scalars]
  (operation #(apply v*s % scalars) matrices))

(defn transpose [m]
  (apply operation vector m))


(defn m*v [m vector]
  (mapv #(dot vector %) m))

(defn m*m [& matrices]
  (reduce (fn [a, b]
            (operation #(m*v (transpose b) %) a))
          matrices))
