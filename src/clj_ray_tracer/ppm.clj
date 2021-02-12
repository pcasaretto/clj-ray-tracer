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
  ["P3"
   (str (:width canvas) " " (:height canvas))
   "255"])


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

(let
    [canvas (-> (canvas/canvas :width 5 :height 3)
                (canvas/assoc-pixel [0 0] ( color/color :red 1.5))
                (canvas/assoc-pixel [2 1] ( color/color :green 0.5))
                (canvas/assoc-pixel [4 2] ( color/color :red -0.5 :blue 1.0)))]
    (canvas->ppm canvas))
