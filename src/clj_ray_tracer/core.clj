(ns clj-ray-tracer.core
  (:require [clj-ray-tracer.canvas :as canvas]
            [clj-ray-tracer.color :as color]
            [clj-ray-tracer.transform :as transform]
            [clj-ray-tracer.ppm :as ppm]
            [clj-ray-tracer.vector :as vector])
  (:gen-class))

(defn clock
  (let
    [canvas (canvas/canvas :width 150 :height 150)
     color (color/color :red 255.0 :green 0.0 :blue 0.0)
     canvas (->> {:x 0 :y 0 :z 0}
             (repeat 12)
             (map #(transform/apply % (transform/translation {:x 50 :y 0 :z 0})))
             (map-indexed #(transform/apply %2 (transform/z-rotation (* %1 (/ Math/PI 6)))))
             (map #(vector/+ {:x 75 :y 75 :z 0} %))
             (map #(vector/update-map (fn [v] (Math/floor v)) %))
             (map #(vector/update-map int %))
             (reduce (fn [c pixel] ( canvas/assoc-pixel c [(:x pixel) (:y pixel)] color) ) canvas))
     bitmap (ppm/canvas->ppm canvas)]
    (println bitmap)))



(defn -main
  [& args])
