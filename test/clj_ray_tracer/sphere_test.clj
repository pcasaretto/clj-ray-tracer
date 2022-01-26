(ns clj-ray-tracer.sphere-test
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]
            [clj-ray-tracer.vector :as vector]
            [clj-ray-tracer.sphere :as sut]
            [clj-ray-tracer.transform :as transform]))


(t/deftest intersects-test
  (t/testing "simple intersection specs"
    (let
        [ray {:origin {:x 0 :y 0 :z -5} :direction {:x 0 :y 0 :z 1}}
         sphere {}]
      (t/is (=
              (list { :t 4.0 :object sphere  } { :t 6.0 :object sphere})
              (sut/intersects sphere ray))))
    (let
        [ray {:origin {:x 0 :y 1 :z -5} :direction {:x 0 :y 0 :z 1}}
         sphere {}]
      (t/is (=
              (list { :t 5.0 :object sphere  } { :t 5.0 :object sphere})
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
              (list { :t -1.0 :object sphere  } { :t 1.0 :object sphere})
              (sut/intersects sphere ray)))))
  (t/testing "a sphere is behind a ray"
    (let
        [ray {:origin {:x 0 :y 0 :z 5} :direction {:x 0 :y 0 :z 1}}
         sphere {}]
      (t/is (=
              (list { :t -6.0 :object sphere } { :t -4.0 :object sphere})
              (sut/intersects sphere ray)))))
  (t/testing "intersecting a scaled sphere with a ray"
    (let
        [ray {:origin {:x 0 :y 0 :z -5} :direction {:x 0 :y 0 :z 1}}
         sphere {:transform (transform/scaling { :x 2 :y 2 :z 2})}]
      (t/is (=
              (list { :t 3.0 :object sphere } { :t 7.0 :object sphere})
              (sut/intersects sphere ray))))))

(t/deftest hit-test
  (t/testing "when all intersections have positive t's"
    (let
        [i1 {:t 1}
         i2 {:t 2}]
      (t/is (=
              i1
              (sut/hit [i1 i2])))))
  (t/testing "when some intersections have negative t's"
    (let
        [i1 {:t -1}
         i2 {:t 1}]
      (t/is (=
              i2
              (sut/hit [i1 i2])))))
  (t/testing "when all intersections have negative t's"
    (let
        [i1 {:t -2}
         i2 {:t -1}]
      (t/is (empty?
              (sut/hit [i1 i2])))))
  (t/testing "the hit is always the lowest non negative intersection"
      (t/is (= {:t 2}
              (sut/hit [{:t 5}
                        {:t 7}
                        {:t -3}
                        {:t 2}])))))

(-> (stest/enumerate-namespace `clj-ray-tracer.sphere) stest/check)
