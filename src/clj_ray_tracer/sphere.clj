(ns clj-ray-tracer.sphere
  (:require [clojure.spec.alpha :as s]
            [clj-ray-tracer.ray :as ray]
            [clj-ray-tracer.vector :as vector]))
(s/def ::sphere (s/keys :req []))

(defn intersects
  [sphere ray]
  (let [direction (:direction ray)
        sphere-to-ray (vector/- (:origin ray) {:x 0 :y 0 :z 0})
        a (vector/dot-product direction direction)
        b (* 2 (vector/dot-product direction sphere-to-ray))
        c (- (vector/dot-product sphere-to-ray sphere-to-ray) 1)
        discriminant (- (* b b) (reduce * [4 a c]))]
    (println a b c discriminant)
    (cond
     (< discriminant 0) '()
     :else (let [t1 (/ (- (- b) (Math/sqrt discriminant)) (* 2 a))
                 t2 (/ (+ (- b) (Math/sqrt discriminant)) (* 2 a))]
                (list t1 t2)))))

(s/fdef intersects
  :args (s/cat :sphere ::sphere :ray ::ray/ray)
  :ret (s/coll-of float?))
