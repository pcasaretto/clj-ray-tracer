(ns clj-ray-tracer.canvas
  (:require [clj-ray-tracer.color :as color]))

(def black (color/color :red 0.0 :green 0.0 :blue 0.0))

(defn canvas [& {:keys [width height]}]
  {
   :width width
   :height height
   :pixels (->> (repeat width black)
                vec
                (repeat height)
                vec)})

(defn update-pixel [canvas, loc, f]
  (update-in canvas (cons :pixels loc) f))

(defn get-pixel [canvas, loc]
  (get-in canvas (cons :pixels loc)))

(defn assoc-pixel [canvas, loc, pixel]
  (assoc-in canvas (cons :pixels loc) pixel))
