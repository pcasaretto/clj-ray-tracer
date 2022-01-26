(ns clj-ray-tracer.ray
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clj-ray-tracer.vector :as vector]
            [clj-ray-tracer.transform :as transform]))

(s/def ::origin ::vector/tuple3d)
(s/def ::direction ::vector/tuple3d)
(s/def ::ray (s/keys :req-un [::origin ::direction]))

(defn position
  [ray time]
  (-> ray
    (update :direction #(vector/scalar-* % time))
    (select-keys [ :origin :direction])
    vals
    (#(apply vector/+ %))))

(s/fdef position
        :args (s/cat :ray ::ray :time (s/and float? #(> % 0)))
        :ret ::vector/tuple3d
        :fn (fn [spec] (cond
                        (= (-> spec :args :time) 0) (= (:ret spec) (-> spec :args :ray))
                        :else true)))

(defn transform
  [ray transformation]
  {:origin (-> ray :origin ( transform/transform-point transformation))
   :direction (-> ray :direction ( transform/transform-vector transformation))})
