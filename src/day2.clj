(ns day2
  (:require [tablecloth.api :as tc]
            [tablecloth.column.api :as tcc]
            [taoensso.timbre :as log]))

(defn ->input
  [resource]
  (tc/dataset resource {:header-row? false
                        :separator " "
                        :key-fn keyword}))

(defn safe?
  [row]
  (log/debug row)
  (reduce (fn [{:keys [not-sorted? previous-level]
                :as result} level]
            (cond
              (= :not-set previous-level) (assoc result :previous-level level)
              (= previous-level level) (reduced :identical)
              (< 3 (abs (- level previous-level))) (reduced :too-far-apart)
              (= :not-set not-sorted?) (assoc result
                                              :not-sorted? (if (< previous-level level) > <)
                                              :previous-level level)
              (not-sorted? previous-level level) (reduced :not-sorted)
              :default (assoc result :previous-level level)))
          {:not-sorted? :not-set
           :previous-level :not-set}
          (filter some? row)))

(let [input (->input "resources/input2.txt")]
  (-> input
      (tc/map-rows (fn [row]
                     {:safe? (safe? (vals row))}))
      (tc/drop-rows (comp
                     #{:not-sorted :identical :too-far-apart}
                     :safe?))
      tc/row-count))

