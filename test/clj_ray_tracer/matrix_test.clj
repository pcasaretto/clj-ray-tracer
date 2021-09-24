(ns clj-ray-tracer.matrix-test
  (:require [clj-ray-tracer.matrix :as sut]
            [clojure.test :as t]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]))

(t/deftest *-test
  (t/testing "simple multiplication"
    (let
        [left [
               [1 2 3 4]
               [5 6 7 8]
               [9 8 7 6]
               [5 4 3 2]]
         right [
                [-2 1 2 3]
                [3 2 1 -1]
                [4 3 6 5]
                [1 2 7 8]]]
      (t/is (=
             [
              [20 22 50 48]
              [44 54 114 108]
              [40 58 110 102]
              [16 26 46 42]]
             (sut/* left right))))
    (let
        [left  [[1 2 3 4]]
         right [
                [1]
                [2]
                [3]
                [4]]]
      (t/is (=
             [[30]]
             (sut/* left right)))))
  (t/testing "identity property"
    (let
        [left (gen/generate (s/gen ::sut/matrix)) 
         right (sut/identity (sut/width left))] 
      (t/is (=
             left
             (sut/* left right)))))
  (t/testing "associative property"
    (let
        [[a b c] (gen/sample (s/gen ::sut/matrix) 3)] 
      (t/is (=
             (sut/* a (sut/* b c))
             (sut/* (sut/* a b) c))))))

(t/deftest transpose-test
  (t/testing "simple transposition specs"
    (let
        [in       [
                    [1 2 3 4]
                    [5 6 7 8]
                    [9 8 7 6]
                    [5 4 3 2]]
         expected [
                    [1 5 9 5]
                    [2 6 8 4]
                    [3 7 7 3]
                    [4 8 6 2]]]
      (t/is (=
             expected
             (sut/transpose in))))
    (let
        [in       [[1 2 3 4]]
         expected [
                    [1]
                    [2]
                    [3]
                    [4]]]
      (t/is (=
             expected
             (sut/transpose in)))))
  (t/testing "transposing twice yields the same matrix"
    (let
        [m (gen/generate (s/gen ::sut/matrix))]
        (t/is (= m (sut/transpose (sut/transpose m)))))))

(t/deftest submatrix-test
  (t/testing "simple submatrix specs"
    (let
        [in       [
                   [1 5 0]
                   [-3 2 7]
                   [0 6 -3]]]
        (t/is (=
                 [[-3 2]
                  [0 6]]
                (sut/submatrix in 0 2))))
    (let
        [in   [
               [-6 1 1 6]
               [-8 5 8 6]
               [-1 0 8 2]
               [-7 1 -1 1]]]
        (t/is (=
                [[-6 1 6]
                 [-8 8 6]
                 [-7 -1 1]]
                (sut/submatrix in 2 1))))))

(t/deftest minor-test
  (t/testing "simple minor specs"
    (let
        [in       [
                   [3 5 0]
                   [2 -1 -7]
                   [6 -1 5]]]

        (t/is (=
                25
                (sut/minor in 1 0))))))


(t/deftest cofactor-test
  (t/testing "simple cofactor specs"
    (let
        [in       [
                   [3 5 0]
                   [2 -1 -7]
                   [6 -1 5]]]
      (t/is (=
             -12
             (sut/cofactor in 0 0)))
      (t/is (=
             -25
             (sut/cofactor in 1 0))))))

(t/deftest determinant-test
  (t/testing "determinant for a 2x2 matrix"
    (let
        [in       [
                   [1 5]
                   [-3 2]]]
        (t/is (= 17 (sut/determinant in)))))
  (t/testing "determinant for a 3x3 matrix"
    (let
        [in       [
                   [1 2 6]
                   [-5 8 -4]
                   [2 6 4]]]
        (t/is (= -196 (sut/determinant in)))))
  (t/testing "determinant for a 4x4 matrix"
    (let
        [in       [
                   [-2 -8 3 5]
                   [-3 1 7 3]
                   [1 2 -9 6]
                   [-6 7 7 -9]]]
        (t/is (= -4071 (sut/determinant in))))))

(t/deftest invertible-test
  (t/testing "test case for a non-invertible 4x4 matrix"
    (let
        [in       [
                   [-4 2 -2 -3]
                   [9 6 2 6]
                   [0 -5 1 -5]
                   [0 0 0 0]]]
        (t/is (not (sut/invertible? in)))))
  (t/testing "test case for a invertible 4x4 matrix"
    (let
        [in       [
                   [6 4 4 4]
                   [5 5 7 6]
                   [4 -9 3 -7]
                   [9 1 7 -6]]]
        (t/is (sut/invertible? in)))))
