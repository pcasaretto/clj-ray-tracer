(ns clj-ray-tracer.sphere
  (:require [clojure.spec.alpha :as s]
            [clj-ray-tracer.ray :as ray]
            [clj-ray-tracer.vector :as vector]
            [clj-ray-tracer.transform :as transform]
            [clj-ray-tracer.matrix :as matrix]))


(defn intersects
  [sphere r]
  (let [
        transform (or (:transform sphere) transform/identity)
        r (ray/transform r (matrix/inverse transform))
        direction (:direction r)
        sphere-to-ray (vector/- (:origin r) {:x 0 :y 0 :z 0})
        a (vector/dot-product direction direction)
        b (* 2 (vector/dot-product direction sphere-to-ray))
        c (- (vector/dot-product sphere-to-ray sphere-to-ray) 1)
        discriminant (- (* b b) (reduce * [4 a c]))]
    (println a b c discriminant)
    (cond
     (< discriminant 0) '()
     :else (let [t1 (/ (- (- b) (Math/sqrt discriminant)) (* 2 a))
                 t2 (/ (+ (- b) (Math/sqrt discriminant)) (* 2 a))]
                (list { :t t1 :object sphere } { :t t2 :object sphere})))))

(defn- better-min-key [k x]
  (if (empty? x) '() (apply min-key k x)))

(defn hit
  [intersections]
  (->> intersections
       (filter (comp pos? :t))
       (better-min-key :t)))

(s/fdef intersects
  :args (s/cat :sphere ::sphere :ray ::ray/ray)
  :ret (s/coll-of float?))
