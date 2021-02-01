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
         v2 (update v1 1 (partial + (* 2 core/epsilon)))]
      (is (not (core/aprox-vector v1 v2)))))
  (testing "vectors with diffs smaller than epsilon are aproximately equal"
    (let
        [v1 (gen/generate (s/gen ::core/tuple3d))
         v2 (update v1 1 (partial + (/ core/epsilon 2)))]
      (is (core/aprox-vector v1 v2)))))

(deftest vector-create-test
  (testing "creating a vector"
    (is (core/aprox-vector
         [3 45 5]
         (vector 3 45 5)))))

(deftest vector-sum-test
  (testing "one argument"
    (is (core/aprox-vector
         [3 4 5]
         (core/vector-sum [3 4 5]))))
  (testing "simple vector sum"
    (is (core/aprox-vector
         [5 9 11]
         (core/vector-sum [3 4 5] [2 5 6]))))
  (testing "identity property"
    (let
      [v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           v1
           (core/vector-sum v1 [0 0 0])))))
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
         [1 -1 -1]
         (core/vector-subtraction [3 4 5] [2 5 6]))))
  (testing "identity property"
    (let
      [v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           v1
           (core/vector-subtraction v1 [0 0 0])))))
  (testing ""
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           [0.0 0.0 0.0]
           (core/vector-subtraction v1 v1))))))

(deftest vector-negation-test
  (testing "simple vector negation"
    (is (core/aprox-vector
         [-3 -4 -5]
         (core/vector-negation [3 4 5]))))
  (testing "a vector and its counterpart cancel each other"
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (core/aprox-vector
           [0.0 0.0 0.0]
           (core/vector-sum v1 (core/vector-negation v1)))))))

(deftest vector-magnitude-test
  (testing "magnitude of a one dimensional unit vector is one"
    (is (=
         1
         (core/vector-magnitude [1 0 0])))
    (is (=
         1
         (core/vector-magnitude [0 1 0])))
    (is (=
         1
         (core/vector-magnitude [0 0 1]))))
  (testing "perfect squares"
    (is (=
          13
          (core/vector-magnitude [3 4 12])))))

(deftest vector-normalization-test
  (testing "simple normalization"
    (is (core/aprox-vector
         [1 0 0]
         (core/vector-normalization [4 0 0])))))
