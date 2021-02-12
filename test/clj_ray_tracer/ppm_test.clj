(ns clj-ray-tracer.ppm-test
  (:require [clj-ray-tracer.ppm :as sut]
            [clj-ray-tracer.canvas :as canvas]
            [clojure.test :as t]
            [clj-ray-tracer.color :as color]))

(t/deftest canvas->ppm-test
  (t/testing "initial header output"
    (let
      [canvas (canvas/canvas :width 5 :height 3)
       ppm (sut/canvas->ppm canvas)]
      (t/is (=
              ["P3" "5 3" "255"]
              (take 3 (clojure.string/split-lines ppm))))))
  (t/testing "initial header output"
    (let
        [canvas (-> (canvas/canvas :width 5 :height 3)
                    (canvas/assoc-pixel [0 0] ( color/color :red 1.5))
                    (canvas/assoc-pixel [2 1] ( color/color :green 0.5))
                    (canvas/assoc-pixel [4 2] ( color/color :red -0.5 :blue 1.0)))
         ppm (sut/canvas->ppm canvas)]
      (t/is (=
              ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0" "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0" "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
              (take 3 (drop 3 (clojure.string/split-lines ppm))))))))
