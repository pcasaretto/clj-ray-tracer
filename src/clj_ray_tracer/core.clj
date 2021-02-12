(ns clj-ray-tracer.core
            [clj-ray-tracer.canvas :as canvas]
            [clj-ray-tracer.color :as color]
            [clj-ray-tracer.ppm :as ppm]
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let
    [canvas (-> (canvas/canvas :width 5 :height 3)
                (canvas/assoc-pixel [0 0] ( color/color :red 1.5))
                (canvas/assoc-pixel [2 1] ( color/color :green 0.5))
                (canvas/assoc-pixel [4 2] ( color/color :red -0.5 :blue 1.0)))]
    bitmap (ppm/canvas->ppm canvas))
  (println bitmap))
