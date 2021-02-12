(ns clj-ray-tracer.canvas
  (:require [clj-ray-tracer.color :as color]
            [clojure.spec.alpha :as s]
            [clojure.test.check.generators :as gen]))

(def black (color/color :red 0.0 :green 0.0 :blue 0.0))

(defn canvas [& {:keys [width height]}]
  {
   :width width
   :height height
   :pixels (->> (repeat width black)
                vec
                (repeat height)
                vec)})

(s/def ::width int?)
(s/def ::height int?)
(s/def ::pixels (s/coll-of (s/coll-of ::color/color)))

(def canvas-gen
  (gen/let [width gen/nat
            height gen/nat
            pixels (gen/vector (gen/vector (s/gen ::color/color) width) height)]
    {:width width
     :height height
     :pixels pixels}))

(s/def ::canvas
  ( s/with-gen
    ( s/and
      ( s/keys :req-un [::width ::height ::pixels])
      (fn [{:keys [:width :height :pixels]}] 
        (= height (count pixels))))
    canvas-gen))

(defn update-pixel [canvas, loc, f]
  (update-in canvas (cons :pixels (reverse loc )) f))

(defn get-pixel [canvas, loc]
  (get-in canvas (cons :pixels (reverse loc))))

(defn assoc-pixel [canvas, loc, pixel]
  (assoc-in canvas (cons :pixels (reverse loc )) pixel))
