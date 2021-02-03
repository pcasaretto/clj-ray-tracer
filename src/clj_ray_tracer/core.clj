(ns clj-ray-tracer.core
  (:require [clojure.spec.alpha :as s]
            [clojure.math.numeric-tower :as math])
  (:gen-class))

(s/def ::valid_double (s/double-in :NaN? false :infinite? false :min -2e9 :max 2e9))
(s/def ::x ::valid_double)
(s/def ::y ::valid_double)
(s/def ::z ::valid_double)
(s/def ::tuple3d (s/keys :req-un [::x ::y ::z]))

(defn update-map [f m]
  (reduce (fn [altered-map [k v]] (assoc altered-map k (f v))) {} m))

(def epsilon 1e-10)

(defn aprox [x y]
  (->
   (- x y)
   math/abs
   (<= epsilon)))

(s/fdef aprox
  :args (s/cat :x ::valid_double :y ::valid_double)
  :ret boolean?)

(defn aprox-vector [v1 v2]
  (->> (merge-with vector v1 v2)
       vals
       (map (partial apply aprox))
       (every? true?)))

(s/fdef aprox-vector
  :args (s/cat :v1 ::tuple3d :v2 ::tuple3d)
  :ret boolean?)

(defn vector-sum
  ( [v1] v1)
  ( [ v1 & more]
    (->> (apply merge-with vector v1 more)
        (update-map (partial apply +)))))

(s/fdef vector-sum
        :args (s/+ ::tuple3d)
        :ret ::tuple3d)

(defn vector-subtraction
  ( [v1] v1)
  ( [ v1 & more]
   (->> (apply merge-with vector v1 more)
        (update-map (partial apply -)))))

(s/fdef vector-subtraction
        :args (s/+ ::tuple3d)
        :ret ::tuple3d)

(defn vector-negation [v]
   (update-map - v))

(s/fdef vector-negation
        :args (s/cat :v ::tuple3d)
        :ret ::tuple3d)

(defn vector-magnitude [v]
  (->> v
       vals
       (map #(math/expt % 2))
       (reduce +)
       (math/sqrt)))

(->> (gen/generate (s/gen ::tuple3d))
  vals
  (map #(math/expt % 2))
  (reduce +)
  (math/sqrt))

(s/fdef vector-magnitude
  :args (s/cat :v ::tuple3d)
  :ret ::valid_double)

(defn vector-normalization [v]
  (let [magnitude (vector-magnitude v)]
    (update-map #(/ % magnitude) v)))

(s/fdef vector-normalization
  :args (s/cat :v ::tuple3d)
  :ret ::tuple3d
  :fn #(aprox 1.0 (vector-magnitude (:ret %))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
