(ns day3)

(def PATTERN #"mul\((\d+),(\d+)\)")

(defn multiplications
  [line]
  (->> line
       (re-seq PATTERN)
       (map (fn [[_ a b]]
              (* (Integer/valueOf a) (Integer/valueOf b))))
       (reduce +)))

#_(->> (slurp "resources/sample3.txt") ; "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))\n"
       multiplications)

#_(->> (slurp "resources/input3.txt")
       (clojure.string/split-lines)
       (map multiplications)
       (reduce +))

(def DO_PATTERN #"do\(\)")
(def DONT_PATTERN #"don't\(\)")

(defn multiplications-bounded
  [line]
  (->> (str "do()" line) ; everything before the first "don't" is an implicit "do"
       (#(clojure.string/split % DONT_PATTERN))
       (map (fn [dont-fragment]
              (clojure.string/split dont-fragment DO_PATTERN)))
       (mapcat rest) ; the first "do" fragment is ignored since it was preceded by a "don't" fragment
       (map multiplications)
       (reduce +)))

(->> (slurp "resources/input3.txt")
     #_(clojure.string/split-lines) ; new lines are just another character in the string
     (vector)
     (map multiplications-bounded)
     (reduce +))  ; 84893551

