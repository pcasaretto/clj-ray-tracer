(ns clj-ray-tracer.core
  (:require [clojure.spec.alpha :as s]
            [clojure.math.numeric-tower :as math])
  (:gen-class))

(s/def ::valid_double (s/double-in :NaN? false :infinite? false :min -2e9 :max 2e9))
(s/def ::tuple3d (s/tuple ::valid_double ::valid_double ::valid_double))

(def epsilon 1e-10)
(defn aprox [x y]
  (->
   (- x y)
   (math/abs)
   (<= epsilon)))

(->
  (- 0.99999 1)
  (math/abs)
  (<= epsilon))

(defn vector-sum
  ( [v1] v1)
  ( [ v1 & more]
    (->> (apply interleave v1 more)
        (partition 2 (inc ( count more)))
        (mapv #(apply + %)))))

(s/fdef vector-sum
        :args (s/+ ::tuple3d)
        :ret ::tuple3d)

(defn vector-subtraction
  ( [v1] v1)
  ( [ v1 & more]
   (->> (apply interleave v1 more)
        (partition 2 (inc ( count more)))
        (mapv #(apply - %)))))

(s/fdef vector-subtraction
        :args (s/+ ::tuple3d)
        :ret ::tuple3d)

(defn vector-negation [v]
   (mapv - v))

(s/fdef vector-negation
        :args (s/cat :v ::tuple3d)
        :ret ::tuple3d)

(defn vector-magnitude [v]
  (->> v
    (map #(math/expt % 2))
    (reduce +)
    (math/sqrt)))

(s/fdef vector-magnitude
  :args (s/cat :v ::tuple3d)
  :ret ::valid_double)

(defn vector-normalization [v]
  (mapv #(/ % (vector-magnitude v)) v))

(s/fdef vector-normalization
  :args (s/cat :v ::tuple3d)
  :ret ::tuple3d
  :fn #(aprox 1.0 (vector-magnitude (:ret %))))

(-> [9.015341061873074E87 -1.3670317029893825E101 -2.6958168392985234E173]
  (vector-magnitude))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
