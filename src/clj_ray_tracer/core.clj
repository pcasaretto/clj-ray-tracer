(ns clj-ray-tracer.core
  (:require [clojure.spec.alpha :as s])
  (:gen-class))

(s/def ::valid_double (s/double-in :NaN? false :infinite? false))
(s/def ::tuple3d (s/tuple ::valid_double ::valid_double ::valid_double))

(defn vector-sum [ v1 v2]
  (->> (interleave v1 v2)
       (partition 2 2)
       (map #(apply + %))))

(s/fdef vector-sum
        :args (s/+ ::tuple3d)
        :ret ::tuple3d)

(defn vector-subtraction [v1 v2]
  (->> (interleave v1 v2)
       (partition 2 2)
       (map #(apply - %))))

(s/fdef vector-subtraction
        :args (s/+ ::tuple3d)
        :ret ::tuple3d)

(defn vector-negation [v1]
   (map - v1))

(s/fdef vector-negation
        :args ::tuple3d
        :ret ::tuple3d)


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
