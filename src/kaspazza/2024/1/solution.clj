(ns kaspazza.2024.1.solution
  (:require
   [clojure.string :as str]))

(def example-data [3 4 4 3 2 5 1 3 3 9 3 3])

(defn read-data
  [filename]
  (let [file-content (slurp filename)
        numbers-raw (str/split file-content #"\s+")]
    (map #(Integer/parseInt %) numbers-raw)))

(defn split-vector [v] [(take-nth 2 v) (take-nth 2 (rest v))])

(defn total-distance
  [d]
  (->> d
       (map sort)
       (apply map (comp abs -))
       (reduce +)))

(defn similarity-score
  [v1 v2]
  (let [freqs (frequencies v2)]
    (->> v1
         (map #(* % (get freqs % 0)))
         (reduce +))))

(comment
  ;;part 1
  (->> example-data
       split-vector
       total-distance)
  (->> "src/kaspazza/2024/1/data.txt"
       read-data
       split-vector
       total-distance)
  ;;part 2
  (->> example-data
       split-vector
       (apply similarity-score))
  (->> "src/kaspazza/2024/1/data.txt"
       read-data
       split-vector
       (apply similarity-score))
  ;
)
