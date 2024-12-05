(ns day4
  (:require [tech.v3.datatype :as dtype]
            [tech.v3.datatype.argops :as ops]
            [tech.v3.tensor :as dtt]
            [tech.v3.tensor.dimensions :as dtd]))

(def N 4)
(def DIRECTIONS
  {:east (for [dy (range N)] [0 dy])
   :west (for [dy (range N)] [0 (* -1 dy)])
   :north (for [dx (range N)] [(* -1 dx) 0])
   :south (for [dx (range N)] [dx 0])
   :north-east (for [dxy (range N)] [(* -1 dxy) dxy])
   :north-west (for [dxy (range N)] [(* -1 dxy) (* -1 dxy)])
   :south-east (for [dxy (range N)] [dxy dxy])
   :south-west (for [dxy (range N)] [dxy (* -1 dxy)])})

(defn ->xy
  [sx idx]
  [(quot idx sx) (mod idx sx)])

(defn ->idx
  [sx [x y]]
  (+ (* sx x) y))

(defn ->one-direction
  [max-x max-y direction [x y]]
  (for [[dx dy] (get DIRECTIONS direction)
        :let [[xi yi] [(+ x dx) (+ y dy)]]
        :when (and (< -1 xi max-x)
                   (< -1 yi max-x))]
    [xi yi]))

(let [sample (->> (slurp "resources/input4.txt")
                  clojure.string/split-lines
                  (map vec)
                  dtt/->tensor)
      dims (dtt/tensor->dimensions sample)
      [sx _] (dtd/strides dims)
      [max-x max-y] (dtd/->2d-shape dims)
      XsIDX (ops/argfilter \X sample)
      XsXY (map (partial ->xy sx) XsIDX)]
  (reduce +
          (for [direction
                #{:north :south :east :west :north-east :north-west :south-east :south-west}]
            (->> (map (partial ->one-direction max-x max-y direction) XsXY)
                 (filter #(= N (count %)))
                 (map #(map (partial ->idx sx) %))
                 (map #(map (partial dtype/get-value sample) %))
                 (map #(apply str %))
                 (filter #(= "XMAS" %))
                 count))))

