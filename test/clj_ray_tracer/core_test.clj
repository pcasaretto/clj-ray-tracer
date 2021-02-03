(ns clj-ray-tracer.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clj-ray-tracer.core :as core]))

(-> (stest/enumerate-namespace `clj-ray-tracer.core) stest/check)

(deftest aprox-vector-test
  (testing "equal vectors are aproximately equal"
    (let
        [v (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector v v))))
  (testing "vectors with diffs larger than epsilon are not aproximately equal"
    (let
        [v1 (gen/generate (s/gen ::core/tuple3d))
         v2 (update v1 :x (partial + (* 2 core/epsilon)))]
      (is (not (core/aprox-vector v1 v2)))))
  (testing "vectors with diffs smaller than epsilon are aproximately equal"
    (let
        [v1 (gen/generate (s/gen ::core/tuple3d))
         v2 (update v1 :x (partial + (/ core/epsilon 2)))]
      (is (core/aprox-vector v1 v2)))))

(deftest vector-sum-test
  (testing "one argument"
    (is (core/aprox-vector
         {:x 3 :y 4 :z 5}
         (core/vector-sum {:x 3 :y 4 :z 5}))))
  (testing "simple vector sum"
    (is (core/aprox-vector
         {:x 5 :y 9 :z 11}
         (core/vector-sum {:x 3 :y 4 :z 5} {:x 2 :y 5 :z 6}))))
  (testing "identity property"
    (let
      [v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           v1
           (core/vector-sum v1 {:x 0 :y 0 :z 0})))))
  (testing "commutative property"
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))
       v2 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           (core/vector-sum v2 v1)
           (core/vector-sum v1 v2)))))
  (testing "associative propery"
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))
       v2 (gen/generate (s/gen ::core/tuple3d))
       v3 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           (core/vector-sum v1 (core/vector-sum v2 v3))
           (core/vector-sum (core/vector-sum v1 v2) v3))))))

(deftest vector-subtration-test
  (testing "simple vector subtraction"
    (is (core/aprox-vector
         {:x -1 :y -1 :z -1}
         (core/vector-subtraction {:x 3 :y 4 :z 5} {:x 4 :y 5 :z 6}))))
  (testing "identity property"
    (let
      [v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           v1
           (core/vector-subtraction v1 {:x 0 :y 0 :z 0})))))
  (testing "subracting a vector from itself results in the zero vector"
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           { :x 0 :y 0 :z 0}
           (core/vector-subtraction v1 v1))))))

(deftest vector-negation-test
  (testing "simple vector negation"
    (is (core/aprox-vector
         { :x -3 :y -4 :z -5}
         (core/vector-negation { :x 3 :y 4 :z 5}))))
  (testing "a vector and its counterpart cancel each other"
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           { :x 0 :y 0 :z 0}
           (core/vector-sum v1 (core/vector-negation v1)))))))

(deftest vector-magnitude-test
  (testing "magnitude of a one dimensional unit vector is one"
    (is (core/aprox
         1
         (core/vector-magnitude {:x 1 :y 0 :z 0})))
    (is (core/aprox
         1
         (core/vector-magnitude {:x 0 :y 1 :z 0})))
    (is (core/aprox
         1
         (core/vector-magnitude {:x 0 :y 0 :z 1}))))
  (testing "perfect squares"
    (is (core/aprox
          13
          (core/vector-magnitude {:x 3 :y 4 :z 12})))))

(deftest vector-normalization-test
  (testing "simple normalization"
    (is (core/aprox-vector
         { :x 1 :y 0 :z 0}
         (core/vector-normalization { :x 4 :y 0 :z 0}))))
  (testing "a normalized vector has magnitude of 1"
    (is (core/aprox
          1
          (-> (gen/generate (s/gen ::core/tuple3d))
              (core/vector-normalization)
              (core/vector-magnitude))))))
