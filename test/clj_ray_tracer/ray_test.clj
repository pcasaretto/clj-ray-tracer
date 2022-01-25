(ns clj-ray-tracer.ray-test
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clj-ray-tracer.ray :as sut]))


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
  (testing "identity property (at time 0, there is no change)"
    (let
      [r (gen/generate (s/gen ::sut/ray))]
      (is (=
           (:origin r)
           (sut/position r 0))))))

(-> (stest/enumerate-namespace `clj-ray-tracer.ray) stest/check)
