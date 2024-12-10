(ns kaspazza.2024.4.solution
  (:require [clojure.string :as str]))

(def puzzle-input
  "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX")

(defn find-xmas
  "find how many xmas appears in string"
  [s]
  (+ (count (re-seq #"SAMX" s)) (count (re-seq #"XMAS" s))))

(defn- diagonal-colls
  [frow rrow]
  (map-indexed (fn [idx el]
                 (vec (concat [el]
                              (map (fn [row idx] (nth row idx))
                                rrow
                                (range (inc idx) (inc (count frow)))))))
               frow))
(defn- top-left-diagonal-colls
  [rows]
  (diagonal-colls (drop-last (first rows)) (rest rows)))

(defn- top-right-diagonal-colls
  [rows]
  (diagonal-colls (reverse (rest (first rows))) (map reverse (rest rows))))

(defn- bottom-left-diagonal-colls
  [rows]
  (diagonal-colls (drop-last (last rows)) (reverse (butlast rows))))

(defn- bottom-right-diagonal-colls
  [rows]
  (diagonal-colls (reverse (rest (last rows)))
                  (map reverse (reverse (butlast rows)))))

(defn xmas-strings
  [rows]
  (let [rows-coll (map #(str/split % #"") rows)
        columns (apply map vector rows-coll)
        left-diagonal (top-left-diagonal-colls rows)
        right-diagonal (top-right-diagonal-colls rows)
        bottom-left-diagonal (bottom-left-diagonal-colls rows)
        bottom-right-diagonal (bottom-right-diagonal-colls rows)
        diagonal (concat left-diagonal
                         right-diagonal
                         (rest bottom-left-diagonal)
                         (rest bottom-right-diagonal))]
    (vec (concat rows
                 (map #(str/join "" %) columns)
                 (map #(str/join "" %) diagonal)))))

(defn xmas-search
  [rows]
  (->> (xmas-strings rows)
       (map find-xmas)
       (reduce +)))

(defn find-mas
  "find how many xmas appears in string"
  [s]
  (+ (count (re-seq #"SAM" s)) (count (re-seq #"MAS" s))))

(defn boards-3x3
  [rows]
  (->> (map-indexed
         (fn [row-idx row]
           (when (nth rows (+ row-idx 2) nil)
             (->> (map-indexed
                    (fn [col-idx _v]
                      (when (nth row (+ col-idx 2) nil)
                        (let [row1 (nth rows row-idx)
                              row2 (nth rows (inc row-idx))
                              row3 (nth rows (inc (inc row-idx)))
                              new-board
                                [[(nth row1 col-idx) (nth row1 (inc col-idx))
                                  (nth row1 (inc (inc col-idx)))]
                                 [(nth row2 col-idx) (nth row2 (inc col-idx))
                                  (nth row2 (inc (inc col-idx)))]
                                 [(nth row3 col-idx) (nth row3 (inc col-idx))
                                  (nth row3 (inc (inc col-idx)))]]]
                          new-board)))
                    row)
                  (remove nil?))))
         rows)
       (remove nil?)
       (apply concat)))

(defn mas?
  [rows]
  (->> rows
       (map find-mas)
       (reduce +)
       pos-int?))

(defn mas-match
  [new-board]
  (let [left-diagonal? (->> (diagonal-colls (drop-last (first new-board))
                                            (rest new-board))
                            (map #(str/join "" %))
                            mas?)
        right-diagonal? (->> (diagonal-colls (reverse (rest (first new-board)))
                                             (map reverse (rest new-board)))
                             (map #(str/join "" %))
                             mas?)]
    (if (and left-diagonal? right-diagonal?) 1 0)))

(defn mas-search
  [rows]
  (->> rows
       boards-3x3
       (map mas-match)
       (reduce +)))

(comment
  ;; Part 1
  (-> puzzle-input
      (str/split #"\n")
      xmas-search)
  (-> "src/kaspazza/2024/4/data.txt"
      slurp
      (str/split #"\n")
      xmas-search)
  ;; Part 2
  (-> puzzle-input
      (str/split #"\n")
      mas-search)
  (-> "src/kaspazza/2024/4/data.txt"
      slurp
      (str/split #"\n")
      mas-search)
  ;
)
