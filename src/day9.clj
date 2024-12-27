(ns day9
  (:require
   [taoensso.timbre :as log]
   [tech.v3.datatype :as dtype]
   [tech.v3.datatype.argops :as ops]))

(def EMPTY-SPACE-ID -1)
(def EMPTY-SPACE-REPR ".")

(defn ->input
  [resource]
  (let [xf (comp
            (map (comp parse-long str))
            (filter some?))]
    (transduce xf conj [] (seq (slurp resource)))))

(defn ->disk-map
  [input]
  (loop [id 0
         current-type :file
         [current-length & remaining] input
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
  (let [length (dtype/ecount disk-map)
        last-idx (dec length)]
    (when (< idx last-idx)
      (loop [current-idx (inc idx)
             current (dtype/get-value disk-map current-idx)]
        (let [next-idx (inc current-idx)]
          (cond
            (= EMPTY-SPACE-ID current) current-idx
            (<= length next-idx) nil
            :default (recur next-idx (dtype/get-value disk-map next-idx))))))))

(defn find-previous-file-block
  [disk-map idx]
  (when (< 0 idx)
    (loop [current-idx (dec idx)
           current (dtype/get-value disk-map current-idx)]
      (cond
        (not= EMPTY-SPACE-ID current) current-idx
        (= 0 current-idx) nil
        :default (let [next-idx (dec current-idx)]
                   (recur next-idx (dtype/get-value disk-map next-idx)))))))

(defn compact
  [disk-map]
  (let [length (dtype/ecount disk-map)]
    (loop [space-idx (find-next-space disk-map -1)
           file-block-idx (find-previous-file-block disk-map length)
           compacted disk-map]
      ;(println (disk-map->str compacted))
      (if (or (nil? space-idx) (<= file-block-idx space-idx))
        compacted
        (do
          (dtype/set-value! compacted space-idx (dtype/get-value compacted file-block-idx))
          (dtype/set-value! compacted file-block-idx EMPTY-SPACE-ID)
          (recur (find-next-space disk-map space-idx)
                 (find-previous-file-block disk-map file-block-idx)
                 compacted))))))

(defn compute-checksum
  [disk-map]
  (reduce (fn [{:keys [current-idx checksum]} current]
            (if (= EMPTY-SPACE-ID current)
              {:current-idx (inc current-idx)
               :checksum checksum}
              {:current-idx (inc current-idx)
               :checksum (+ checksum (* current-idx current))}))
          {:current-idx 0
           :checksum 0}
          disk-map))

(let [input (->input "resources/input9.txt")
      disk-map (->disk-map input)
      compacted (compact disk-map)]
  (compute-checksum compacted))

(defn find-file-block-start-idx
  [disk-map file-block-end-idx]
  (let [id (dtype/get-value disk-map file-block-end-idx)]
    (ops/index-of disk-map id)))

(defn find-space-end-idx
  [disk-map space-start-idx]
  (let [length (dtype/ecount disk-map)
        last-idx (dec length)]
    (loop [current-idx (inc space-start-idx)]
      (cond
        (< last-idx current-idx) (dec current-idx)
        (not= EMPTY-SPACE-ID (dtype/get-value disk-map current-idx)) (dec current-idx)
        :default (recur (inc current-idx))))))

(defn ->length [start-idx end-idx]
  (+ 1 (- end-idx start-idx)))

(defn find-available-space
  [disk-map file-block-start-idx file-block-length]
  (loop [space-start-idx (find-next-space disk-map -1)]
    (if (or (nil? space-start-idx) (<= file-block-start-idx space-start-idx))
      nil
      (let [space-end-idx (find-space-end-idx disk-map space-start-idx)
            space-length (->length space-start-idx space-end-idx)]
        (if (<= file-block-length space-length)
          [space-start-idx space-length]
          (recur (find-next-space disk-map space-end-idx)))))))

#_(let [input (->input "resources/sample9.txt")
        disk-map (->disk-map input)]
    (println (disk-map->str disk-map))
    (find-available-space disk-map (ops/index-of disk-map 9) 4))

(defn move-to-available-space
  [disk-map file-block-start-idx file-block-length space-idx]
  (let [to-move (dtype/indexed-buffer (range file-block-start-idx (+ file-block-start-idx file-block-length)) disk-map)
        target (dtype/indexed-buffer (range space-idx (+ space-idx file-block-length)) disk-map)]
    (dtype/copy! to-move target)
    (dtype/set-constant! disk-map file-block-start-idx file-block-length EMPTY-SPACE-ID)))

(let [input (->input "resources/sample9.txt")
      disk-map (->disk-map input)]
  (let [length (dtype/ecount disk-map)]
    (loop [file-block-end-idx (find-previous-file-block disk-map length)
           file-block-start-idx (find-file-block-start-idx disk-map file-block-end-idx)
           files []
           compacted disk-map]
      ;(log/debug :file-block-start-idx file-block-start-idx :wfile-block-end-idx file-block-end-idx)
      ;(println (disk-map->str compacted))
      (let [file-block-length (->length file-block-start-idx file-block-end-idx)
            available-space (find-available-space compacted file-block-start-idx file-block-length)
            updated-files (conj files [file-block-start-idx file-block-end-idx available-space])]
        (when-let [[space-idx _] available-space]
          (move-to-available-space compacted file-block-start-idx file-block-length space-idx))
        (if (zero? file-block-start-idx)
          (compute-checksum compacted)
          (if-let [next-file-block-end-idx (find-previous-file-block compacted file-block-start-idx)]
            (recur next-file-block-end-idx
                   (find-file-block-start-idx compacted next-file-block-end-idx)
                   updated-files
                   compacted)
            (compute-checksum compacted)))))))
