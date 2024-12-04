(ns day3)

(def PATTERN #"mul\((\d+),(\d+)\)")

(defn multiplications
  [line]
  (->> line
       (re-seq PATTERN)
       (map (fn [[_ a b]]
              (* (Integer/valueOf b) (Integer/valueOf a))))
       (reduce +)))

(->> (slurp "resources/sample3.txt") ; "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))\n"
     multiplications)

(->> (slurp "resources/input3.txt")
     (clojure.string/split-lines)
     (map multiplications)
     (reduce +))

