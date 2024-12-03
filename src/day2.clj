(ns day2
  (:require [tablecloth.api :as tc]
            [tablecloth.column.api :as tcc]))

(defn ->input
  [resource]
  (tc/dataset resource {:header-row? false
                        :separator " "
                        ;:key-fn keyword
                        }))

(defn safe?
  [row]
  ;(prn row)
  (reduce (fn [{:keys [not-sorted? previous-value]
                :as result} value]
            (cond
              (nil? previous-value) (assoc result :previous-value value)
              (= value previous-value) (reduced :identical)
              (nil? not-sorted?) (assoc result
                                        :not-sorted? (if (< previous-value value) > <)
                                        :previous-value value)
              (not-sorted? previous-value value) (reduced :not-sorted)
              (< 3 (abs (- value previous-value))) (reduced :too-far-apart)
              :default (assoc result :previous-value value)))
          {:not-sorted? nil
           :previous-value nil}
          (filter some? row)))

(let [input (->input "resources/input2.txt")]
  (-> input
      (tc/map-rows (fn [row]
                     {:safe? (safe? (vals row))}))
      (tc/drop-rows (comp
                     #{:not-sorted :identical :too-far-apart}
                     :safe?))
      tc/row-count))

