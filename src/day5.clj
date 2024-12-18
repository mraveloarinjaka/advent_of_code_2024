(ns day5)

(defn line->ordering-rule
  [line]
  (when-let [[_ p-before p-after] (re-find #"(\d+)\|(\d+)" line)]
    [p-before p-after]))

(defn ->ordering-rules
  [input]
  (->> input
       (map line->ordering-rule)
       (filter some?)
       (group-by first)
       (map (fn [[k v]] {k (set (mapv second v))}))
       (into {})))

(defn line->pages-update
  [line]
  (or (re-matches #"\d+" line)
      (and (re-find #"\d+," line) (clojure.string/split line #","))))

(defn ->pages-update
  [input]
  (->> input
       (map line->pages-update)
       (filter some?)))

(defn right-order?
  [ordering-rules pages-update]
  (loop [[current & remaining] pages-update
         pages-so-far #{}]
    (let [previous-updates-breaking-rules
          (clojure.set/intersection (get ordering-rules current #{}) pages-so-far)]
      ;(println {:current current
      ;          :remaining remaining
      ;          :rules (get ordering-rules current)
      ;          :so-far pages-so-far})
      (if (= #{} previous-updates-breaking-rules)
        (if (seq remaining)
          (recur remaining (conj pages-so-far current))
          {:right-order? true
           :updates-breaking-rules {}})
        (let [update-breaking-rule (first previous-updates-breaking-rules)]
          {:right-order? false
           :updates-breaking-rules {current update-breaking-rule
                                    update-breaking-rule current}})))))

(let [input (->> (slurp "resources/sample5.txt")
                 clojure.string/split-lines)
      ordering-rules (->ordering-rules input)
      pages-updates (->pages-update input)]
  (->> pages-updates
       (filter (comp :right-order? (partial right-order? ordering-rules)))
       (map (fn [updates]
              (Integer/valueOf (nth updates (quot (count updates) 2)))))
       (reduce +)))

(let [input (->> (slurp "resources/input5.txt")
                 clojure.string/split-lines)
      ordering-rules (->ordering-rules input)
      pages-updates (->pages-update input)]
  (->> pages-updates
       (map #(vector % (right-order? ordering-rules %)))
       (remove (comp :right-order? second))
       ; we keep fixing the order until the updates are correctly sorted
       (map (fn fix-pages-updates [[updates {fixed? :right-order?
                                             :keys [updates-breaking-rules]}]]
              (if fixed?
                updates
                (let [fixed-updates (map (fn fix-update
                                           [update]
                                           (get updates-breaking-rules update update)) updates)]
                  (recur [fixed-updates (right-order? ordering-rules fixed-updates)])))))
       (map (fn [updates]
              (Integer/valueOf (nth updates (quot (count updates) 2)))))
       (reduce +)))

