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
  (t/testing "identity property"
    (let
        [in (sut/identity 3)]
        (t/is (=
                in
                (sut/transpose in))))))
