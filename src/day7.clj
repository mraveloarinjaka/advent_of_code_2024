(ns day7
  (:require [clojure.math.combinatorics :as cx]
            [taoensso.timbre :as log]))

(defn ->input
  [resource]
  (->> (slurp resource)
       clojure.string/split-lines
       (map (fn ->equation [line]
              (let [[test-result numbers] (clojure.string/split line #":")
                    parsed-test-result (parse-long test-result)
                    parsed-numbers (->> (clojure.string/split numbers #" ")
                                        (remove clojure.string/blank?)
                                        (map parse-long))]
                [parsed-test-result parsed-numbers])))))

(def OPERATORS ['+ '*])

#_(mapv #(eval (list % 40 2)) OPERATORS)

(defn compute-operators-seq
  [operators length]
  (cx/selections operators length))

(def ^:dynamic *compute-operators-seq* compute-operators-seq)

(defn evaluate-operators-seq
  [target numbers ops]
  (->> (reduce (fn apply-op [[arg1 arg2 & remaining] op]
                 (let [result (eval (list op arg1 arg2))]
                   (if (< target result)
                     (reduced (list result))
                     (conj remaining result)))) numbers ops)
       first))

(evaluate-operators-seq 1 '(40 2) (repeat 1 '+))

(defn evaluate
  [operators result numbers]
  (let [nb-operators (dec (count numbers))
        evaluator (partial evaluate-operators-seq result numbers)
        lower-bound (evaluator (repeat nb-operators '+))
        upper-bound (evaluator (repeat nb-operators '*))
        operators-selections (*compute-operators-seq* operators nb-operators)]
    (when (<= lower-bound result upper-bound)
      (some #(= result (evaluator %)) operators-selections))))

(->> (nth (->input "resources/input7.txt") 10)
     ((fn [[result numbers]]
        (evaluate OPERATORS result numbers))))

(evaluate OPERATORS 556488358421 '(794 98 33 6 917 7))
#_(evaluate OPERATORS 5423661 '(5 2 8 8 91 4 6 7 3 40 8 6))

(def ^:dynamic *sampler* identity)

(defn compute-calibration
  [resource]
  (let [xf (comp
            (map (fn [input]
                   (println input)
                   input))
            *sampler*
            (map (fn [[result numbers]]
                   (when (evaluate OPERATORS result numbers) result)))
            (filter some?))]
    (transduce xf + (->input resource))))

(compute-calibration "resources/sample7.txt")

#_(binding [*sampler* (take 10)]
    (compute-calibration "resources/input7.txt"))

(comment

  (require '[criterium.core :as ct])

  (binding [*compute-operators-seq* (memoize compute-operators-seq)]
    (ct/with-progress-reporting
      (ct/bench
       (compute-calibration "resources/sample7.txt"))))

  (comment))
;(cx/selections OPERATORS 3)sample
