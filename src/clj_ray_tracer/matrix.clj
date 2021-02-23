(ns clj-ray-tracer.matrix
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

(defn * [left right]
  (for [line-number (range 0 (height left))]
    (for [column-number (range 0 (width right))]
      (->> (interleave (line left line-number) (column right column-number))
           (partition 2)
           (map (partial apply clojure.core/*))
           (reduce +)))))

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
            1
            0))))))

(defn transpose [m]
  (vec
    (for [i (range 0 (width m))]
      (vec
       (for [j (range 0 (height m))]
          (get-in m [j i]))))))
