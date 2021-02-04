(ns clj-ray-tracer.color-test
  (:require [clj-ray-tracer.color :as sut]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clojure.test :as t]))

(-> (stest/enumerate-namespace `clj-ray-tracer.color) stest/check)

(t/deftest color-test
  (t/testing "a color is a three component map"
    (let [color (sut/color :red 0.84 :green -0.5 :blue 1.7)]
      (t/is (= 0.84 (:red color)))
      (t/is (= -0.5 (:green color)))
      (t/is (= 1.7 (:blue color))))))

(t/deftest color-sum-test
  (t/testing "identity property"
    (let
        [c1 (gen/generate (s/gen ::sut/color))]
      (t/is (sut/=
              c1
              (sut/sum c1 {:red 0 :green 0 :blue 0})))))
  (t/testing "commutative property"
    (let
        [
         c1 (gen/generate (s/gen ::sut/color))
         v2 (gen/generate (s/gen ::sut/color))]
      (t/is (sut/=
              (sut/sum v2 c1)
              (sut/sum c1 v2)))))
  (t/testing "associative propery"
    (let
        [
         c1 (gen/generate (s/gen ::sut/color))
         v2 (gen/generate (s/gen ::sut/color))
         v3 (gen/generate (s/gen ::sut/color))]
      (t/is (sut/=
              (sut/sum c1 (sut/sum v2 v3))
              (sut/sum (sut/sum c1 v2) v3))))))

(t/deftest subtract-test
  (t/testing "simple color subtraction"
    (t/is (sut/=
            {:red -1 :green -1 :blue -1}
            (sut/subtract {:red 3 :green 4 :blue 5} {:red 4 :green 5 :blue 6}))))
  (t/testing "identity property"
    (let
        [c1 (gen/generate (s/gen ::sut/color))]
      (t/is (sut/=
              c1
              (sut/subtract c1 {:red 0 :green 0 :blue 0})))))
  (t/testing "subracting a vector from itself results in the zero vector"
    (let
        [
         c1 (gen/generate (s/gen ::sut/color))]
      (t/is (sut/=
              { :red 0.0 :green 0.0 :blue 0.0}
              (sut/subtract c1 c1))))))

(t/deftest scalar-multiply-test
  (t/testing "simple scalar multiplication"
    (t/is (sut/=
           {:red 0.4 :green 0.6 :blue 0.8}
           (sut/scalar-multiply {:red 0.2 :green 0.3 :blue 0.4} 2)))))

(t/deftest multiply-test
  (t/testing "simple multiplication"
    (t/is (sut/=
           {:red 0.9 :green 0.2 :blue 0.04}
           (sut/multiply {:red 1 :green 0.2 :blue 0.4} {:red 0.9 :green 1 :blue 0.1})))))
