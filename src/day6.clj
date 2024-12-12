(ns day6
  (:require [tech.v3.datatype :as dtype]
            [tech.v3.datatype.argops :as ops]
            [tech.v3.tensor :as dtt]
            [tech.v3.tensor.dimensions :as dtd]))

(def DIRECTIONS #{\< \> \^ \v})

(defn ->row-col
  [dims index]
  (let [[row-stride _] (dtd/strides dims)]
    [(quot index row-stride) (mod index row-stride)]))

(defn ->index
  [dims [row col]]
  (let [[row-stride _] (dtd/strides dims)]
    (+ (* row-stride row) col)))

(def BLOCK \#)

(defn ->input
  [resource]
  (let [input (->> (slurp resource)
                   (clojure.string/split-lines)
                   (mapv vec)
                   dtt/->tensor)
        [start] (ops/argfilter DIRECTIONS input)
        dims (dtt/tensor->dimensions input)]
    {:position (->row-col dims start)
     :direction (dtype/get-value input start)
     :blocks (set (map (partial ->row-col dims) (ops/argfilter BLOCK input)))
     :lab-map input}))

(def MOVEMENTS
  {\^ [-1 0]
   \> [0 1]
   \v [1 0]
   \< [0 -1]})

(defn move
  [position direction]
  (let [[row col] position
        [dr dc] (get MOVEMENTS direction)]
    [(+ row dr) (+ col dc)]))

(defn is-out?
  [input [row col]]
  (let [dims (dtt/tensor->dimensions (:lab-map input))
        [row-max col-max] (dtd/shape dims)]
    (not (and (< -1 row row-max)
              (< -1 col col-max)))))

(defn is-occupied?
  [{:keys [blocks]} position]
  (blocks position))

(defn turn!
  [{:keys [direction]
    :as input}]
  (let [new-direction
        (condp = direction
          \^ \>
          \> \v
          \v \<
          \< \^
          (throw "unknown direction"))
        turn (assoc input :direction new-direction)]
    ;(println :turn! turn)
    turn))

(let [input (->input "resources/input6.txt")
      dims (dtt/tensor->dimensions (:lab-map input))]
  (loop [{:keys [position direction]
          :as current} input
         steps 0
         visited #{}]
    (let [new-position (move position direction)]
      (cond
        (is-out? input position)
        #_> [current (count visited)]
        (is-occupied? input new-position)
        #_> (recur (turn! current) steps visited)
        :else (recur (-> current
                         (assoc :position new-position)
                         (update :lab-map #(dtype/set-value! % (->index dims position) steps)))
                     (inc steps)
                     (conj visited position))))))
