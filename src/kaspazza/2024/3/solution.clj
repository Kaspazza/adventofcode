(ns kaspazza.2024.3.solution
  (:require [clojure.string :as str]))

(def corrupted-input
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def corrupted-input2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn str->num
  "Turns a string containing two numbers like `5,4` into collection of integers"
  [str-nums]
  (map #(Integer/parseInt %) (str/split str-nums #",")))

(defn find-mul-fns
  "Finds mul functions in corrupted memory"
  [s]
  (re-seq #"mul\((\d+,\d+)\)" s))

(defn apply-conditional-instr
  "Applys conditional instructions to not evaluate code in a string after 'don't()' untill there is 'do()' instruction."
  [s]
  (str/replace s #"((?s)don't\(\)\s*.*?do\(\))|(?s)don't\(\)\s*.*" ""))


(comment
  ;;Part 1
  (->> corrupted-input
       find-mul-fns
       (map (comp str->num first rest))
       (map #(apply * %))
       (reduce +))
  (->> "src/kaspazza/2024/3/data.txt"
       slurp
       find-mul-fns
       (map (comp str->num first rest))
       (map #(apply * %))
       (reduce +))
  ;;Part 2
  (->> corrupted-input2
       apply-conditional-instr
       find-mul-fns
       (map (comp str->num first rest))
       (map #(apply * %))
       (reduce +))
  (->> "src/kaspazza/2024/3/data.txt"
       slurp
       apply-conditional-instr
       find-mul-fns
       (map (comp str->num first rest))
       (map #(apply * %))
       (reduce +))
  ;
)
