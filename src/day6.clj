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
(def EMPTY \.)

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
     :empty-spaces (dtype/->buffer (ops/argfilter EMPTY input))
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

(defn patrol
  [input]
  (let [dims (dtt/tensor->dimensions (:lab-map input))]
    (loop [{:keys [position direction]
            :as current} input
           steps 0
           visited #{}
           loop-detection #{}]
      (let [new-position (move position direction)]
        (cond
          (is-out? input position) #_> [:exited current (count visited)]
          (loop-detection [position direction]) #_> [:loop current (count visited)]
          (is-occupied? input new-position) #_> (recur (turn! current) steps visited loop-detection)
          :else (recur (-> current
                           (assoc :position new-position)
                           (update :lab-map (fn mark-position
                                              [map-to-update]
                                              (dtype/set-value! map-to-update (->index dims position) steps))))
                       (inc steps)
                       (conj visited position)
                       (conj loop-detection [position direction])))))))

(let [{:keys [empty-spaces]
       :as input} (->input "resources/input6.txt")
      dims (dtt/tensor->dimensions (:lab-map input))]
  (count (for [empty-space-idx empty-spaces
               :let [empty-space (->row-col dims empty-space-idx)
                     updated-with-new-obstruction (update input :blocks conj empty-space)
                     [patrol-result] (patrol updated-with-new-obstruction)]
               :when (= patrol-result :loop)]
           empty-space)))


