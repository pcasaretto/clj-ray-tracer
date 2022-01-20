(ns clj-ray-tracer.transform
    (:refer-clojure :exclude [apply])
    (:require [clj-ray-tracer.matrix :as matrix]))

(defn point->matrix
  [point]
  [ [(:x point)] [(:y point)] [(:z point)] [1]])

(defn matrix->point
  [matrix]
  {:x (get-in matrix [0 0])
   :y (get-in matrix [1 0])
   :z (get-in matrix [2 0])})

(defn apply
  [point & transforms]
  (let
      [resulting-transforms (->> transforms vec rseq (reduce matrix/*))]
    (->> point
           point->matrix
           (matrix/* resulting-transforms)
           matrix->point)))

(defn translation
  [spec]
  (let [{:keys [x y z]} spec]
    [[1 0 0 x]
     [0 1 0 y]
     [0 0 1 z]
     [0 0 0 1]]))

(defn scaling
  [spec]
  (let [{:keys [x y z]} spec]
    [[x 0 0 0]
     [0 y 0 0]
     [0 0 z 0]
     [0 0 0 1]]))

(defn x-rotation
  [r]
  [[1 0 0 0]
   [0 (Math/cos r) (- (Math/sin r)) 0]
   [0 (Math/sin r) (Math/cos r) 0]
   [0 0 0 1]])

(defn y-rotation
  [r]
  [[(Math/cos r) 0 (Math/sin r) 0]
   [0 1 0 0]
   [(- (Math/sin r) ) 0 (Math/cos r) 0]
   [0 0 0 1]])

(defn z-rotation
  [r]
  [[(Math/cos r) (- (Math/sin r)) 0 0]
   [(Math/sin r) (Math/cos r) 0 0]
   [0 0 1 0]
   [0 0 0 1]])

(defn shearing
  [xy xz yx yz zx zy]
  [[1 xy xz 0]
   [yx 1 yz 0]
   [zx zy 1 0]
   [0 0 0 1]])

(defn foo
  [a & rest]
  (println a (rseq (vec rest))))
