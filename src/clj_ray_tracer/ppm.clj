(ns clj-ray-tracer.ppm
  (:require [clj-ray-tracer.canvas :as canvas]
            [clj-ray-tracer.color :as color]))

(defn scale-pixel [color-value]
  (->> color-value
       (* 255)
       (max 0.0)
       (min 255.0)
       Math/round))

(defn pixel-line [pixels]
  (->> pixels
      (map
        #(vector
          (scale-pixel (:red %))
          (scale-pixel (:green %))
          (scale-pixel (:blue %))))
      flatten))

(defn ^:private ppm-header [canvas]
  ["P3" (str (:width canvas) " " (:height canvas)) "255"])


(defn canvas->ppm [canvas]
  (clojure.string/join \newline
    (vector
      (clojure.string/join \newline (ppm-header canvas))
      (->> canvas
           :pixels
           (map pixel-line)
           (map vec)
           (map #(clojure.string/join " " %))
           vec
           (clojure.string/join \newline)))))
