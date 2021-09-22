(ns clj-ray-tracer.vector-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clj-ray-tracer.vector :as sut]))

(-> (stest/enumerate-namespace `clj-ray-tracer.vector) stest/check)

(deftest ish?-test
  (testing "equal vectors are aproximately equal"
    (let
        [v (gen/generate (s/gen ::sut/tuple3d))]
      (is (sut/ish? v v))))
  (testing "vectors with diffs larger than epsilon are not aproximately equal"
    (let
        [v1 (gen/generate (s/gen ::sut/tuple3d))
         v2 (update v1 :x (partial + (* 2 sut/epsilon)))]
      (is (not (sut/ish? v1 v2)))))
  (testing "vectors with diffs smaller than epsilon are aproximately equal"
    (let
        [v1 (gen/generate (s/gen ::sut/tuple3d))
         v2 (update v1 :x (partial + (/ sut/epsilon 2)))]
      (is (sut/ish? v1 v2)))))

(deftest +-test
  (testing "one argument"
    (is (sut/ish?
         {:x 3 :y 4 :z 5}
         (sut/+ {:x 3 :y 4 :z 5}))))
  (testing "simple vector sum"
    (is (sut/ish?
         {:x 5 :y 9 :z 11}
         (sut/+ {:x 3 :y 4 :z 5} {:x 2 :y 5 :z 6}))))
  (testing "identity property"
    (let
      [v1 (gen/generate (s/gen ::sut/tuple3d))]
      (is (sut/ish?
           v1
           (sut/+ v1 {:x 0 :y 0 :z 0})))))
  (testing "commutative property"
    (let
      [
       v1 (gen/generate (s/gen ::sut/tuple3d))
       v2 (gen/generate (s/gen ::sut/tuple3d))]
      (is (sut/ish?
           (sut/+ v2 v1)
           (sut/+ v1 v2)))))
  (testing "associative propery"
    (let
      [
       v1 (gen/generate (s/gen ::sut/tuple3d))
       v2 (gen/generate (s/gen ::sut/tuple3d))
       v3 (gen/generate (s/gen ::sut/tuple3d))]
      (is (sut/ish?
           (sut/+ v1 (sut/+ v2 v3))
           (sut/+ (sut/+ v1 v2) v3))))))

(deftest vector-subtration-test
  (testing "simple vector subtraction"
    (is (sut/ish?
         {:x -1 :y -1 :z -1}
         (sut/- {:x 3 :y 4 :z 5} {:x 4 :y 5 :z 6}))))
  (testing "identity property"
    (let
      [v1 (gen/generate (s/gen ::sut/tuple3d))]
      (is (sut/ish?
           v1
           (sut/- v1 {:x 0 :y 0 :z 0})))))
  (testing "subracting a vector from itself results in the zero vector"
    (let
      [
       v1 (gen/generate (s/gen ::sut/tuple3d))]
      (is (sut/ish?
           { :x 0 :y 0 :z 0}
           (sut/- v1 v1))))))

(deftest negate-test
  (testing "simple vector negation"
    (is (sut/ish?
         { :x -3 :y -4 :z -5}
         (sut/negate { :x 3 :y 4 :z 5}))))
  (testing "a vector and its counterpart cancel each other"
    (let
      [
       v1 (gen/generate (s/gen ::sut/tuple3d))]
      (is (sut/ish?
           { :x 0 :y 0 :z 0}
           (sut/+ v1 (sut/negate v1)))))))

(deftest magnitude-test
  (testing "magnitude of a one dimensional unit vector is one"
    (is (sut/aprox
         1
         (sut/magnitude {:x 1 :y 0 :z 0})))
    (is (sut/aprox
         1
         (sut/magnitude {:x 0 :y 1 :z 0})))
    (is (sut/aprox
         1
         (sut/magnitude {:x 0 :y 0 :z 1}))))
  (testing "perfect squares"
    (is (sut/aprox
          13
          (sut/magnitude {:x 3 :y 4 :z 12})))))

(deftest normalize-test
  (testing "simple normalization"
    (is (sut/ish?
         { :x 1 :y 0 :z 0}
         (sut/normalize { :x 4 :y 0 :z 0}))))
  (testing "a normalized vector has magnitude of 1"
    (is (sut/aprox
          1
          (-> (gen/generate (s/gen ::sut/tuple3d))
              (sut/normalize)
              (sut/magnitude))))))
