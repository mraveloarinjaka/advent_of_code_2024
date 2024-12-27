(ns day9
  (:require
   [taoensso.timbre :as log]
   [tech.v3.datatype :as dtype]))

(def EMPTY-SPACE-ID -1)
(def EMPTY-SPACE-REPR ".")

(defn ->input
  [resource]
  (let [xf (comp
            (map (comp parse-long str))
            (filter some?))])
  (transduce xf into [] (seq (slurp resource))))

(defn ->disk-map
  [input]
  (loop [id 0
         current-type :file
         ([current-length & remaining] input)
         disk-map []]
    (let [updated-disk-map (case current-type
                             :free-space (apply conj disk-map (take current-length (cycle [EMPTY-SPACE-ID])))
                             :file (apply conj disk-map (take current-length (cycle [id]))))]
      (if (seq remaining)
        (let [next-id (case current-type
                        :free-space id
                        :file (inc id))
              next-type (case current-type
                          :free-space :file
                          :file :free-space)]
          (recur next-id
                 next-type
                 remaining
                 updated-disk-map))
        (dtype/make-list :long updated-disk-map)))))

(defn disk-map->str
  [decoded]
  (reduce #(str %1 (if (= EMPTY-SPACE-ID %2) EMPTY-SPACE-REPR %2)) "" decoded))

(defn find-next-space
  [disk-map idx]
  (let [length (dtype/ecount disk-map)]
    (loop [current-idx (inc idx)
           current (dtype/get-value disk-map current-idx)]
      (if (or (<= length current-idx) (= EMPTY-SPACE-ID current))
        current-idx
        (let [next-idx (inc current-idx)]
          (recur next-idx (dtype/get-value disk-map next-idx)))))))

(defn find-previous-file-block
  [disk-map idx]
  (loop [current-idx (dec idx)
         current (dtype/get-value disk-map current-idx)]
    (if (or (< current-idx 0) (not= EMPTY-SPACE-ID current))
      current-idx
      (let [next-idx (dec current-idx)]
        (recur next-idx (dtype/get-value disk-map next-idx))))))

(defn compact
  [disk-map]
  (let [length (dtype/ecount disk-map)]
    (loop [space-idx (find-next-space disk-map -1)
           file-block-idx (find-previous-file-block disk-map length)
           compacted disk-map]
      ;(println (disk-map->str compacted))
      (if (<= file-block-idx space-idx)
        compacted
        (do
          (dtype/set-value! compacted space-idx (dtype/get-value compacted file-block-idx))
          (dtype/set-value! compacted file-block-idx EMPTY-SPACE-ID)
          (recur (find-next-space disk-map space-idx)
                 (find-previous-file-block disk-map file-block-idx)
                 compacted))))))

(let [input (->input "resources/input9.txt")
      disk-map (->disk-map input)
      compacted (compact disk-map)]
  (reduce (fn [{:keys [current-idx checksum]} current]
            (if (= EMPTY-SPACE-ID current)
              {:current-idx (inc current-idx)
               :checksum checksum}
              {:current-idx (inc current-idx)
               :checksum (+ checksum (* current-idx current))}))
          {:current-idx 0
           :checksum 0}
          compacted))
