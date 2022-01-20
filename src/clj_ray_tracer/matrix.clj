(ns clj-ray-tracer.matrix
  (:refer-clojure :exclude [* identity])
  (:require [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]))

(defn width [m]
  (count (get m 0)))

(defn height [m]
  (count m))

(defn line [m line-number]
  (get m line-number))

(defn column [m column-number]
  (mapv #(get % column-number) m))

(defn element-apply [m f]
  (mapv #(mapv f %) m))

(defn * [left right]
  (vec
    (for [line-number (range 0 (height left))]
      (vec
        (for [column-number (range 0 (width right))]
          (->> (interleave (line left line-number) (column right column-number))
              (partition 2)
              (map (partial apply clojure.core/*))
              (reduce +)))))))

(s/def ::double
  (s/double-in :NaN? false :infinite? false))

(def matrix-gen
  (gen/let [size gen/nat]
     (gen/vector (gen/vector (s/gen ::double) size) size)))

(s/def ::matrix
  (s/with-gen
    (s/coll-of (s/coll-of ::double))
    (constantly matrix-gen)))

(defn identity [size]
  (vec
    (for [i (range 0 size)]
      (vec
        (for [j (range 0 size)]
          (if (= i j)
            1.0
            0.0))))))

(defn transpose [m]
  (vec
    (for [i (range 0 (width m))]
      (vec
       (for [j (range 0 (height m))]
          (get-in m [j i]))))))

(defn if-different [n]
  #(if (not= %1) %2))

(defn submatrix [m exclude-row exclude-column]
  (vec
    (keep-indexed
       (fn [current-row row]
         (if (not= current-row exclude-row)
           (vec
            (keep-indexed (fn [current-column column] (if (not= current-column exclude-column) column)) row))))
       m)))

(declare determinant)
(defn minor [m exclude-row exclude-column]
  (-> m
      (submatrix exclude-row exclude-column)
      (determinant)))

(defn- diagonal?
  [row column]
  (even? (+ row column)))

(defn cofactor
  [m row column]
  (let
      [minor (minor m row column)]
      (if (diagonal? row column) minor (- minor))))

(defn determinant2 [m]
    (clojure.core/-
        (clojure.core/* (get-in m [ 0 0 ]) (get-in m [ 1 1]))
        (clojure.core/* (get-in m [ 1 0 ]) (get-in m [ 0 1]))))

(defn determinant3 [m]
  (->>
    (range (height m))
    (map #(clojure.core/* (get-in m [0 %]) (cofactor m 0 %)))
    (reduce +)))

(defn determinant [m]
  (cond
    (= (width m) 2) (determinant2 m)
    (>= (width m) 3) (determinant3 m)))

(defn invertible? [m]
  (not (zero? (determinant m))))

(defn cofactor-matrix [m]
  (let [size (height m)]
    (vec
        (for [i (range 0 size)]
          (vec
            (for [j (range 0 size)]
              (cofactor m i j)))))))

(defn inverse [m]
  (let
      [det (determinant m)]
    (-> m
          cofactor-matrix
          transpose
          (element-apply #(/ % det)))))

(def in [
          [-5.0 2.0 6.0 -8.0]
          [1.0 -5.0 1.0 8.0]
          [7.0 7.0 -6.0 -7.0]
          [1.0 -3.0 7.0 4.0]])

(defn vec->matrix
  [v]
  [(conj (vec (vals (select-keys v [:x :y :z]))) 0)])

(vec->matrix {:x 1 :y 34 :z 9})

(-> in
  inverse
  (* in))
