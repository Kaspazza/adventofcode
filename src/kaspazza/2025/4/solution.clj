(ns kaspazza.2025.4.solution
  (:require
   [clojure.string :as str]))

(def example
  "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defn calculate-neighbours-positions
  [row column max-column max-row]
  (remove (fn [{:keys [column row]}]
            (or (< column 0) (< row 0) (> column max-column) (> row max-row)))
          [{:column (- column 1)
            :row row}
           {:column (+ column 1)
            :row row}
           {:column column
            :row (- row 1)}
           {:column column
            :row (+ row 1)}
           {:column (- column 1)
            :row (- row 1)}
           {:column (+ column 1)
            :row (+ row 1)}
           {:column (- column 1)
            :row (+ row 1)}
           {:column (+ column 1)
            :row (- row 1)}]))

(defn neighbour-paper-touching
  [{:keys [column row]} rows]
  (if (= (get-in rows [row column]) "@") 1 0))

(defn calculate-movable-paper
  [rows]
  (let [height (count rows)
        width (count (first rows))]
    (->> rows
         (map-indexed
          (fn [row-idx row]
            (map-indexed
             (fn [cell-idx cell]
               (if (= "@" cell)
                 (let [neighbours (->>
                                    (calculate-neighbours-positions row-idx cell-idx width height)
                                    (map (fn [neighbour] (neighbour-paper-touching neighbour rows)))
                                    (apply +))]
                   (if (<= neighbours 3)
                     {:row row-idx
                      :cell cell-idx}
                     false))
                 false))
             row)))
         (map #(filter map? %))
         (apply concat))))

(defn remove-paper
  [rows
   {:keys [row cell]
    :as _cell-to-remove}]
  (assoc-in rows [row cell] "."))

(defn calculate-movable-paper-with-removal
  [rows]
  (loop [removed 0
         rows rows]
    (let [cels-to-remove (calculate-movable-paper rows)]
      (if (seq cels-to-remove)
        (recur
         (+ removed
            (->> cels-to-remove
                 count))
         (reduce (fn [acc cell-to-remove] (remove-paper acc cell-to-remove)) rows cels-to-remove))
        removed))))

(comment

 ;;Part 1
 ;;
 (->> (-> example
          (str/split #"\n"))
      (map
       #(str/split % #""))
      vec
      calculate-movable-paper
      count)
 #_13

 (->> (-> "src/kaspazza/2025/4/data.txt"
          slurp
          (str/split #"\n"))
      (map
       #(str/split % #""))
      vec
      calculate-movable-paper
      count)
 #_1543


 ;;Part 2
 (->> (-> example
          (str/split #"\n"))
      (map
       #(str/split % #""))
      vec
      calculate-movable-paper-with-removal)
 #_43

 (->> (-> "src/kaspazza/2025/4/data.txt"
          slurp
          (str/split #"\n"))
      (map
       #(str/split % #""))
      vec
      calculate-movable-paper-with-removal)
 #_9038


 ;
)

