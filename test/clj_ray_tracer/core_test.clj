(ns clj-ray-tracer.core-test
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clj-ray-tracer.core :as core]))

(-> (stest/enumerate-namespace `clj-ray-tracer.core) stest/check)

(deftest vector-create
  (testing "creating a vector"
    (is (= (vector 3 45 5) [3 45 5]))))

(deftest vector-arithmetic-sum
  (testing "simple vector sum"
    (is (=
         (core/vector-sum [3 4 5] [2 5 6])
         [5 9 11])))
  (testing "identity property"
    (let
      [v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (=
           (core/vector-sum v1 [0 0 0])
           v1))))
  (testing "commutative property"
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))
       v2 (gen/generate (s/gen ::core/tuple3d))]
      (is (=
           (core/vector-sum v1 v2)
           (core/vector-sum v2 v1)))))
  (testing "associative propery"
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))
       v2 (gen/generate (s/gen ::core/tuple3d))
       v3 (gen/generate (s/gen ::core/tuple3d))]
      (is (=
           (core/vector-sum (core/vector-sum v1 v2) v3)
           (core/vector-sum v1 (core/vector-sum v2 v3)))))))

(deftest vector-arithmetic-subtration
  (testing "simple vector subtraction"
    (is (=
         (core/vector-subtraction [3 4 5] [2 5 6])
         [1 -1 -1])))
  (testing "identity property"
    (let
      [v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (=
           (core/vector-subtraction v1 [0 0 0])
           v1))))
  (testing ""
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (=
           (core/vector-subtraction v1 v1)
           [0.0 0.0 0.0])))))

(deftest vector-arithmetic-negation
  (testing "simple vector negation"
    (is (=
         (core/vector-negation [3 4 5])
         [-3 -4 -5])))
  (testing "a vector and its counterpart cancel each other"
    (let
      [
       v1 (gen/generate (s/gen ::core/tuple3d))]
      (is (=
           (core/vector-sum v1 (core/vector-negation v1))
           [0.0 0.0 0.0])))))
