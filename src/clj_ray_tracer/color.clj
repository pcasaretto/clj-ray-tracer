(ns clj-ray-tracer.color
  (:require [clojure.spec.alpha :as s]
            [clojure.math.numeric-tower :as math]
            [clj-ray-tracer.core :as core]))

(s/def ::red (s/double-in :NaN? false :infinite? false))
(s/def ::green (s/double-in :NaN? false :infinite? false))
(s/def ::blue (s/double-in :NaN? false :infinite? false))
(s/def ::color (s/keys :req-un [::red ::green ::blue]))

(defn color [& {:keys [red green blue]
                :or {red 0.0
                     green 0.0
                     blue 0.0}}]
  {:red red :green green :blue blue})

(s/fdef color
  :args (s/keys* :req [::red ::green ::blue])
  :ret ::color)

(defn = [c1 c2]
  (->> (merge-with vector c1 c2)
       vals
       (map (partial apply core/aprox))
       (every? true?)))

(s/fdef =
  :args (s/cat :c1 ::color :c2 ::color)
  :ret boolean?)

(defn make-operation [f]
  (fn
    ( [c1] c1)
    ( [ c1 & more]
      (->> (apply merge-with (comp flatten vector) c1 more)
           (core/update-map (partial apply f))))))

(def sum (make-operation +))

(s/fdef sum
  :args (s/+ ::color)
  :ret ::color)

(def subtract (make-operation -))

(s/fdef subtract
  :args (s/+ ::color)
  :ret ::color)

(defn scalar-multiply [color factor]
  (core/update-map (partial * factor) color))

(s/fdef subtract
  :args (s/cat :color ::color :factor int?)
  :ret ::color)

(def multiply (make-operation *))

(s/fdef multiply
  :args (s/+ ::color)
  :ret ::color)
