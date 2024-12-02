(ns day1
  (:require [tablecloth.api :as tc]
            [tablecloth.column.api :as tcc]))

(defn ->input
  [resource]
  (let [input (tc/dataset resource {:header-row? false
                                    :separator " "
                                    :key-fn keyword})

        columns (tc/column-names input)
        left (input (first columns))
        right (input (last columns))]
    (tc/dataset {:left left
                 :right right})))

(defn total-distance
  [input]
  (let [left (tcc/sort-column (input :left))
        right (tcc/sort-column (input :right))]
    (-> (tcc/- left right)
        (tcc/abs)
        (tcc/sum))))

(total-distance (->input "resources/sample1.txt"))
(total-distance (->input "resources/input1_1.txt"))

(defn similarity-score
  [input]
  (let [left (input :left)
        right (input :right)
        right-frequencies (frequencies right)
        similarity (tcc/column-map left (fn [l] (get right-frequencies l 0)))
        score (tcc/* left similarity)]
    (tcc/reduce-+ score)))

(similarity-score (->input "resources/sample1.txt"))
(similarity-score (->input "resources/input1_1.txt"))

(comment

  (comment))
