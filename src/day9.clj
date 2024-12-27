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
        (dtype/make-list :int64 updated-disk-map)))))

(defn disk-map->str
  [decoded]
  (reduce #(str %1 (if (= EMPTY-SPACE-ID %2) EMPTY-SPACE-REPR %2)) "" decoded))

(defn find-next-space
  [{:keys [disk-map length last-idx]} idx]
  #_(let [where-to-look (dtype/sub-buffer disk-map (inc idx))
          length (dtype/ecount where-to-look)
          idx (ops/index-of where-to-look EMPTY-SPACE-ID)]
      (when (< idx length) idx))
  (when (< idx last-idx)
    (let [current-idx (inc idx)
          current (dtype/get-value disk-map current-idx)
          next-idx (inc current-idx)]
      (cond
        (= EMPTY-SPACE-ID current) current-idx
        (<= length next-idx) nil
        :default (let [where-to-look (dtype/sub-buffer disk-map next-idx)
                       next-space-idx (+ next-idx (ops/index-of where-to-look EMPTY-SPACE-ID))]
                   next-space-idx)))
    #_(loop [current-idx (inc idx)
             current (dtype/get-value disk-map current-idx)]
        (let [next-idx (inc current-idx)]
          (cond
            (= EMPTY-SPACE-ID current) current-idx
            (<= length next-idx) nil
            :default (recur next-idx (dtype/get-value disk-map next-idx)))))))

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

(defn ->disk-map-data
  [disk-map]
  (let [length (dtype/ecount disk-map)]
    {:disk-map disk-map
     :length length
     :last-idx (dec length)}))

(defn compact
  [disk-map]
  (let [data (->disk-map-data disk-map)]
    (loop [space-idx (find-next-space data -1)
           file-block-idx (find-previous-file-block disk-map length)
           compacted disk-map]
      ;(println (disk-map->str compacted))
      (if (or (nil? space-idx) (<= file-block-idx space-idx))
        compacted
        (do
          (dtype/set-value! compacted space-idx (dtype/get-value compacted file-block-idx))
          (dtype/set-value! compacted file-block-idx EMPTY-SPACE-ID)
          (recur (find-next-space data space-idx)
                 (find-previous-file-block compacted file-block-idx)
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

(let [input (->input "resources/sample9.txt")
      disk-map (->disk-map input)
      compacted (compact disk-map)]
  (compute-checksum compacted))

(defn find-file-block-start-idx
  [disk-map file-block-end-idx]
  (let [id (dtype/get-value disk-map file-block-end-idx)]
    (ops/index-of disk-map id)))

(defn find-space-end-idx
  [disk-map space-start-idx]
  #_(let [length (dtype/ecount disk-map)
          last-idx (dec length)]
      (loop [current-idx (inc space-start-idx)]
        (cond
          (< last-idx current-idx) (dec current-idx)
          (not= EMPTY-SPACE-ID (dtype/get-value disk-map current-idx)) (dec current-idx)
          :default (recur (inc current-idx)))))
  (let [where-to-look (dtype/sub-buffer disk-map (inc space-start-idx))
        ; why = ?
        ; there is either a bug or I do not understand how index-of works
        space-end-idx (ops/index-of where-to-look = EMPTY-SPACE-ID)]
    #_(println :where-to-look where-to-look
               :find-space-end-idx [space-start-idx space-end-idx])
    (+ space-start-idx space-end-idx)))

(defn ->length [start-idx end-idx]
  (+ 1 (- end-idx start-idx)))

(defn find-available-space
  [disk-map file-block-start-idx file-block-length]
  (let [data (->disk-map-data disk-map)]
    (loop [space-start-idx (find-next-space data -1)]
      (if (or (nil? space-start-idx) (<= file-block-start-idx space-start-idx))
        nil
        (let [space-end-idx (find-space-end-idx disk-map space-start-idx)
              space-length (->length space-start-idx space-end-idx)]
          (if (<= file-block-length space-length)
            [space-start-idx space-length]
            (recur (find-next-space data space-end-idx))))))))

#_(let [input (->input "resources/sample9.txt")
        disk-map (->disk-map input)]
    (println (disk-map->str disk-map))
    (find-available-space disk-map (ops/index-of disk-map 9) 4))

(defn move-to-available-space
  [disk-map file-block-start-idx file-block-length space-idx]
  (let [to-move (dtype/sub-buffer disk-map file-block-start-idx file-block-length)
        target (dtype/sub-buffer disk-map space-idx file-block-length)]
    (dtype/copy! to-move target)
    (dtype/set-constant! disk-map file-block-start-idx file-block-length EMPTY-SPACE-ID)))

(defn compact-bis
  [disk-map]
  (let [length (dtype/ecount disk-map)]
    (loop [file-block-end-idx (find-previous-file-block disk-map length)
           file-block-start-idx (find-file-block-start-idx disk-map file-block-end-idx)
           compacted disk-map]
      ;(log/debug :file-block-start-idx file-block-start-idx :wfile-block-end-idx file-block-end-idx)
      ;(println (disk-map->str compacted))
      (let [file-block-length (->length file-block-start-idx file-block-end-idx)
            available-space (find-available-space compacted file-block-start-idx file-block-length)]
        (when-let [[space-idx _] available-space]
          (move-to-available-space compacted file-block-start-idx file-block-length space-idx))
        (if (zero? file-block-start-idx)
          (compute-checksum compacted)
          (if-let [next-file-block-end-idx (find-previous-file-block compacted file-block-start-idx)]
            (recur next-file-block-end-idx
                   (find-file-block-start-idx compacted next-file-block-end-idx)
                   compacted)
            (compute-checksum compacted)))))))

(let [input (->input "resources/sample9.txt")
      disk-map (->disk-map input)]
  (compact-bis disk-map))

;(let [buffer (dtype/make-list :int64 (range 4 10))]
;    (println buffer)
;    (ops/index-of buffer not= 5))

(comment

  (require '[clj-async-profiler.core :as prof])

  (prof/profile (dotimes [i 1000]
                  (let [input (->input "resources/sample9.txt")
                        disk-map (->disk-map input)]
                    (compact-bis disk-map))))

  (prof/profile (let [input (->input "resources/input9.txt")
                      disk-map (->disk-map input)]
                  (compact-bis disk-map)))

  (prof/serve-ui 8282)

  (comment))

