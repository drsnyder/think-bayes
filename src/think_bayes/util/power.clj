(ns think-bayes.util.power)

(defn power-seq [start stop alpha]
  (map #(Math/pow % (* -1.0 alpha)) (range start stop)))

(defn power-pmf [start stop alpha]
  (into {}
       (for [idx (range start stop)]
         [idx (Math/pow idx (* -1.0 alpha))])))
