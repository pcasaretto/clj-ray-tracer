(ns clj-ray-tracer.color
  (:require [clojure.spec.alpha :as s]
            [clojure.math.numeric-tower :as math]
            [clj-ray-tracer.vector :as vector]
            [clojure.spec.gen.alpha :as gen]))

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
       (map (partial apply vector/aprox))
       (every? true?)))

(s/fdef =
  :args (s/cat :c1 ::color :c2 ::color)
  :ret boolean?)

(defn make-operation [f]
  (fn
    ( [c1] c1)
    ( [ c1 & more]
      (->> (apply merge-with (comp flatten vector) c1 more)
           (vector/update-map (partial apply f))))))

(def + (make-operation clojure.core/+))

(s/fdef +
  :args (s/+ ::color)
  :ret ::color)

(def - (make-operation clojure.core/-))

(s/fdef -
  :args (s/+ ::color)
  :ret ::color)

(defn scalar-multiply [color factor]
  (vector/update-map (partial * factor) color))

(s/fdef scalar-multiply
  :args (s/cat :color ::color :factor int?)
  :ret ::color)

(def * (make-operation clojure.core/*))

(s/fdef *
  :args (s/+ ::color)
  :ret ::color)
