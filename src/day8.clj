 (ns day8
   (:require [clojure.math.combinatorics :as cx]
             [com.climate.claypoole :as cp]
             [taoensso.timbre :as log]
             [tech.v3.datatype :as dtype]
             [tech.v3.datatype.argops :as ops]
             [tech.v3.tensor :as dtt]
             [tech.v3.tensor.dimensions :as dtd]
             [thi.ng.geom.core :as gcore]
             [thi.ng.geom.types :as gtypes]
             [thi.ng.geom.vector :as vec]
             [thi.ng.math.core :as mcore]))

(defn ->row-col
  [dims index]
  (let [[row-stride _] (dtd/strides dims)]
    [(quot index row-stride) (mod index row-stride)]))

(defn ->index
  [dims [row col]]
  (let [[row-stride _] (dtd/strides dims)]
    (+ (* row-stride row) col)))

(def ANTENNAS #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9
                \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
                \A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z})

(defn ->input
  [resource]
  (let [input (->> (slurp resource)
                   (clojure.string/split-lines)
                   (mapv vec)
                   dtt/->tensor)
        dims (dtt/tensor->dimensions input)
        antennas-idxs (ops/arggroup-by #(get ANTENNAS % :empty) input)]
    {:antennas-idxs antennas-idxs
     :antennas-map input
     :dims dims}))

(defn valid?
  [dims v]
   ;(println v)
  (let [[row col] v
        [max-row max-col] (:shape dims)
        idx (->index dims [row col])]
    (and (< -0.5 row max-row)
         (< -0.5 col max-col)
         v)))

(def ^:dynamic *with-resonant-harmonics* false)

(defn ->antinodes
  [dims v1 v2]
  (let [[sx] (:strides dims)
        dv (mcore/- v2 v1)]
    (apply concat (for [n (if *with-resonant-harmonics* (range sx) [1])
                        :let [dvn (gcore/scale dv n)
                              a1 (mcore/- v1 dvn)
                              a2 (mcore/+ v2 dvn)]
                        :when (or (valid? dims a1)
                                  (valid? dims a2))]
                    [a1 a2]))))

#_(let [v1 (vec/vec2 1 1)
        v2 (vec/vec2 2 2)]
    (->antinodes 0 v1 v2))

#_(cx/combinations (range 1 9) 2)

(defn antenna-idxs->antinodes
  [dims antennas-idxs]
  (filter (partial valid? dims)
          (apply concat
                 (for [[idx1 idx2] (cx/combinations antennas-idxs 2)
                       :let [v1 (vec/vec2 (->row-col dims idx1))
                             v2 (vec/vec2 (->row-col dims idx2))]]
                   (->antinodes dims v1 v2)))))

(binding [*with-resonant-harmonics* true]
  (let [input (->input "resources/input8.txt")]
    (when *with-resonant-harmonics* (println "*with-resonant-harmonics*"))
    (->> (:antennas-idxs input)
         (reduce-kv (fn [res k v]
                      (case k
                        :empty res
                        (into res (antenna-idxs->antinodes (:dims input) v))))
                    #{})
         count)))
