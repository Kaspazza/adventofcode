(ns kaspazza.2024.2.solution
  (:require [clojure.string :as str]))

(def reports
  ["7 6 4 2 1" "1 2 7 8 9" "9 7 6 2 1" "1 3 2 4 5" "8 6 4 4 1" "1 3 6 7 9"])

(defn read-file-reports
  [filename]
  (-> filename
      slurp
      (str/split #"\n")))

(defn str->num-report
  "Turns one report from string form into numbers vector. Expects each level in the `report` string to be separated by one whitespace"
  [report]
  (map #(Integer/parseInt %) (str/split report #" ")))

(defn levels-relation
  [n1 n2]
  (cond (< n1 n2) :increasing
        (> n1 n2) :decreasing
        (= n1 n2) :equal
        :else {:status :failed,
               :message "Comparison failed",
               :data {:n1 n1, :n2 n2}}))

(defn levels-diff-safe?
  [l1 l2]
  (let [diff (abs (- l1 l2))] (and (not= diff 0) (<= diff 3))))

(defn levels-safe?
  [expected-relation l1 l2]
  (and (= expected-relation (levels-relation l1 l2)) (levels-diff-safe? l1 l2)))

(defn report-safe?
  "Returns true if report is safe, false otherwise.
   Report is a sequential collection of integers"
  [report]
  (let [expected-relation (apply levels-relation (take 2 report))
        report-analysis-result
          (reduce (fn [[expected-relation prev-level :as acc] level]
                    (if (levels-safe? expected-relation prev-level level)
                      (-> acc
                          pop
                          (conj level))
                      (reduced :unsafe)))
            [expected-relation (first report)]
            (rest report))]
    (not= :unsafe report-analysis-result)))

(defn drop-nth [n coll] (concat (take n coll) (drop (inc n) coll)))

(defn dampener-report-safe?
  "Tolerate a single bad level"
  [report]
  (->> report
       count
       range
       (mapv #(drop-nth % report))
       (some report-safe?)))

(comment
  ;;part 1
  (->> reports
       (map (comp report-safe? str->num-report))
       (filter true?)
       count)
  (->> "src/kaspazza/2024/2/data.txt"
       read-file-reports
       (map (comp report-safe? str->num-report))
       (filter true?)
       count)
  ;;part 2
  (->> reports
       (map (comp dampener-report-safe? str->num-report))
       (filter true?)
       count)
  (->> "src/kaspazza/2024/2/data.txt"
       read-file-reports
       (map (comp dampener-report-safe? str->num-report))
       (filter true?)
       count)
  ;
)
