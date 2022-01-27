(ns clj-ray-tracer.ray-test
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clj-ray-tracer.ray :as sut]
            [clj-ray-tracer.transform :as transform]))


(t/deftest position-test
  (t/testing "simple position specs"
    (let
        [ray {:origin {:x 2 :y 3 :z 4} :direction {:x 1 :y 0 :z 0}}
         time 0]
      (t/is (=
              {:x 2 :y 3 :z 4}
              (sut/position ray time))))
    (let
        [ray {:origin {:x 2 :y 3 :z 4} :direction {:x 1 :y 0 :z 0}}
         time 1]
      (t/is (=
              {:x 3 :y 3 :z 4}
              (sut/position ray time)))))
  (t/testing "identity property (at time 0, there is no change)"
    (let
      [r (gen/generate (s/gen ::sut/ray))]
      (t/is (=
             (:origin r)
             (sut/position r 0))))))

(t/deftest transform-test
  (t/testing "translating a ray"
    (let
        [ray {:origin {:x 1 :y 2 :z 3} :direction {:x 0 :y 1 :z 0}}
         transformation (transform/translation {:x 3 :y 4 :z 5})]
      (t/is (=
              {:origin {:x 4 :y 6 :z 8} :direction {:x 0 :y 1 :z 0}}
              (sut/transform ray transformation)))))
  (t/testing "scaling a ray"
    (let
        [ray {:origin {:x 1 :y 2 :z 3} :direction {:x 0 :y 1 :z 0}}
         transformation (transform/scaling {:x 2 :y 3 :z 4})]
      (t/is (=
              {:origin {:x 2 :y 6 :z 12} :direction {:x 0 :y 3 :z 0}}
              (sut/transform ray transformation))))))

(-> (stest/enumerate-namespace `clj-ray-tracer.ray) stest/check)
