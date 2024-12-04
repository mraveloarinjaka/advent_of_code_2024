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
  ;(log/debug row)
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
          (vals row)))

(def UNSAFE #{:not-sorted :identical :too-far-apart})
(def unsafe? (comp some? UNSAFE :safe?))

(defn safe-when-resilient?
  [row]
  (if (unsafe? row)
    (let [resilient (for [[col _] (dissoc row :safe?)
                          :let [row-to-check (dissoc row col)
          still-not-safe-checks (for [col-to-exclude (keys row-to-check)
                                      :let [row-without-one-level (dissoc row-to-check col-to-exclude)
                                            unsafe ((comp some? UNSAFE safe?) row-without-one-level)]
                                      :while unsafe]
                                  unsafe)]
      ;(log/debug :still-not-safe-checks still-not-safe-checks)
      (< (count still-not-safe-checks) max-nb-checks))
    true))

(let [input (->input "resources/input2.txt")]
  (-> input
      (tc/map-rows (fn [row]
                     {:safe? (safe? row)}))
      ;(tc/drop-rows (comp UNSAFE :safe?))
      (tc/map-rows (fn [row]
                     {:safe-when-resilient? (safe-when-resilient? row)}))
      (tc/select-rows :safe-when-resilient?)
      tc/row-count))

