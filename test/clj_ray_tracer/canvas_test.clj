(ns clj-ray-tracer.canvas-test
  (:require [clj-ray-tracer.canvas :as sut]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clj-ray-tracer.color :as color]
            [clojure.test :as t]))

(t/deftest canvas-test
  (t/testing "a canvas is a three component map"
    (let [canvas (sut/canvas :width 10 :height 20)]
      (t/is (= 10 (:width canvas)))
      (t/is (= 20 (:height canvas)))
      (t/is (= 20 (count (:pixels canvas))))
      (t/is (->> canvas
                :pixels
                (map count)
                (every? (partial = 10))))
      (let
          [black (color/color :red 0.0 :green 0.0 :blue 0.0)]
          (t/is (every?
                 #(every? (partial = black) %)
                 (:pixels canvas)))))))

(t/deftest get-pixel-test
  (t/testing "get pixel returns the pixel at the destination"
    (let [canvas (sut/canvas :width 10 :height 20)]
      (t/is (= {:red 0.0 :green 0.0 :blue 0.0} (sut/get-pixel canvas [0 0]))))))

(t/deftest assoc-pixel-test
  (t/testing "assoc pixel sets the pixel at the destination"
    (let [
          canvas (sut/canvas :width 10 :height 20)
          pixel  (gen/generate (s/gen ::color/color))]
      (t/is (= pixel (-> canvas
                         (sut/assoc-pixel [2 3] pixel)
                         (sut/get-pixel [2 3])))))))

(t/deftest update-pixel-test
  (t/testing "update pixel updates the pixel at the destination using the function given"
    (let [
          canvas (sut/canvas :width 10 :height 20)
          f #(assoc % :red 0.89)]
      (t/is (= 0.89 (-> canvas
                        (sut/update-pixel [2 3] f)
                        (sut/get-pixel [2 3])
                        :red))))))
