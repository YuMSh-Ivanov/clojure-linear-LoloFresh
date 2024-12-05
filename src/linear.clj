(ns linear)

(defn operation [f & vector]
  (apply mapv f vector))

(def v+ (partial operation +))

(def v- (partial operation -))

(def v* (partial operation *))

(def vd (partial operation /))

(defn dot [& vectors]
  (if (empty? vectors)
    0
    (apply + (apply v* vectors))))

(defn v*s [vector & scalars]
  (let [prod (apply * scalars)]
    (mapv #(* % prod) vector)))

(def m+ (partial operation v+))

(def m- (partial operation v-))

(def m* (partial operation v*))

(def md (partial operation vd))

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
