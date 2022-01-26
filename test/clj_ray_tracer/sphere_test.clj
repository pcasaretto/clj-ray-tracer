(ns clj-ray-tracer.sphere-test
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clj-ray-tracer.vector :as vector]
            [clj-ray-tracer.sphere :as sut]))


(t/deftest intersects-test
  (t/testing "simple intersection specs"
    (let
        [ray {:origin {:x 0 :y 0 :z -5} :direction {:x 0 :y 0 :z 1}}
         sphere {}]
      (t/is (=
              '(4.0 6.0)
              (sut/intersects sphere ray))))
    (let
        [ray {:origin {:x 0 :y 1 :z -5} :direction {:x 0 :y 0 :z 1}}
         sphere {}]
      (t/is (=
              '(5.0 5.0)
              (sut/intersects sphere ray))))
    (let
        [ray {:origin {:x 0 :y 2 :z -5} :direction {:x 0 :y 0 :z 1}}
         sphere {}]
      (t/is (empty? (sut/intersects sphere ray)))))
  (t/testing "a ray originates inside the sphere"
    (let
        [ray {:origin {:x 0 :y 0 :z 0} :direction {:x 0 :y 0 :z 1}}
         sphere {}]
      (t/is (=
              '(-1.0 1.0)
              (sut/intersects sphere ray)))))
  (t/testing "a sphere is behind a ray"
    (let
        [ray {:origin {:x 0 :y 0 :z 5} :direction {:x 0 :y 0 :z 1}}
         sphere {}]
      (t/is (=
              '(-6.0 -4.0)
              (sut/intersects sphere ray))))))

(-> (stest/enumerate-namespace `clj-ray-tracer.sphere) stest/check)
