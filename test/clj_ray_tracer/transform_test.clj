(ns clj-ray-tracer.transform-test
  (:require  [clojure.test :as t]
             [clojure.math.numeric-tower :as math]
             [clj-ray-tracer.transform :as sut]
             [clj-ray-tracer.matrix :as matrix]
             [clj-ray-tracer.vector :as vector]))


(t/deftest apply-transformations-test
  (t/testing "translation"
    (let
        [transformation (sut/translation { :x 5 :y -3 :z 2})
         point {:x -3 :y 4 :z 5}]
      (t/is (vector/ish? {:x 2 :y 1 :z 7}
               (sut/apply point transformation))))

    (let
        [transformation (matrix/inverse (sut/translation { :x 5 :y -3 :z 2}))
         point {:x -3 :y 4 :z 5}]
      (t/is (vector/ish? {:x -8 :y 7 :z 3}
               (sut/apply point transformation)))))

  (t/testing "scaling"
    (let
        [transformation (sut/scaling { :x 2 :y 3 :z 4})
         point {:x -4 :y 6 :z 8}]
      (t/is (vector/ish? {:x -8 :y 18 :z 32}
               (sut/apply point transformation))))
    (let
        [transformation (matrix/inverse (sut/scaling { :x 2 :y 3 :z 4}))
         point {:x -4 :y 6 :z 8}]
      (t/is (vector/ish? {:x -2 :y 2 :z 2}
               (sut/apply point transformation)))))

  (t/testing "x-rotation"
    (let
        [transformation (sut/x-rotation (/ Math/PI 4))
         point {:x 0 :y 1 :z 0}]
      (t/is (vector/ish? {:x 0 :y (-> 2 math/sqrt (/ 2)) :z (-> 2 math/sqrt (/ 2))}
               (sut/apply point transformation))))
    (let
        [transformation (sut/x-rotation (/ Math/PI 2))
         point {:x 0 :y 1 :z 0}]
      (t/is (vector/ish? {:x 0 :y 0 :z 1}
               (sut/apply point transformation)))))

  (t/testing "y-rotation"
    (let
        [transformation (sut/y-rotation (/ Math/PI 4))
         point {:x 0 :y 0 :z 1}]
      (t/is (vector/ish? {:x (-> 2 math/sqrt (/ 2)) :y 0 :z (-> 2 math/sqrt (/ 2))}
               (sut/apply point transformation))))
    (let
        [transformation (sut/y-rotation (/ Math/PI 2))
         point {:x 0 :y 0 :z 1}]
      (t/is (vector/ish? {:x 1 :y 0 :z 0}
               (sut/apply point transformation)))))

  (t/testing "z-rotation"
    (let
        [transformation (sut/z-rotation (/ Math/PI 4))
         point {:x 0 :y 1 :z 0}]
      (t/is (vector/ish? {:x (- (-> 2 math/sqrt (/ 2))) :y (-> 2 math/sqrt (/ 2)) :z 0}
               (sut/apply point transformation))))
    (let
        [transformation (sut/z-rotation (/ Math/PI 2))
         point {:x 0 :y 1 :z 0}]
      (t/is (vector/ish? {:x -1 :y 0 :z 0}
               (sut/apply point transformation)))))

  (t/testing "shearing"
    (let [point {:x 2 :y 3 :z 4}]
        (let [transformation (sut/shearing 1 0 0 0 0 0)]
          (t/is (vector/ish? {:x 5 :y 3 :z 4} (sut/apply point transformation))))
        (let [transformation (sut/shearing 0 1 0 0 0 0)]
          (t/is (vector/ish? {:x 6 :y 3 :z 4} (sut/apply point transformation))))
        (let [transformation (sut/shearing 0 0 1 0 0 0)]
          (t/is (vector/ish? {:x 2 :y 5 :z 4} (sut/apply point transformation))))
        (let [transformation (sut/shearing 0 0 0 1 0 0)]
          (t/is (vector/ish? {:x 2 :y 7 :z 4} (sut/apply point transformation))))
        (let [transformation (sut/shearing 0 0 0 0 1 0)]
          (t/is (vector/ish? {:x 2 :y 3 :z 6} (sut/apply point transformation))))
        (let [transformation (sut/shearing 0 0 0 0 0 1)]
          (t/is (vector/ish? {:x 2 :y 3 :z 7} (sut/apply point transformation))))))

  (t/testing "multiple transformations at once"
    (let
        [t1 (sut/x-rotation (/ Math/PI 2))
         t2 (sut/scaling { :x 5 :y 5 :z 5})
         t3 (sut/translation { :x 10 :y 5 :z 7})
         point {:x 1 :y 0 :z 1}]
      (t/is (vector/ish? {:x 15 :y 0 :z 7}
               (sut/apply point t1 t2 t3))))))
