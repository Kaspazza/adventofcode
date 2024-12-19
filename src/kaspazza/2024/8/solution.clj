(ns kaspazza.2024.8.solution
  (:require [clojure.string :as str]))

(def antennas-map
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(def ignore-el ".")

(defn unordered-two-combinations
  [coll]
  (loop [f (first coll)
         rem (rest coll)
         acc []]
    (if (empty? rem)
      acc
      (recur (first rem)
             (rest rem)
             (->> rem
                  (map #(vector f %))
                  (concat acc))))))

(defn row-els
  [row]
  (some->> row
           (map-indexed (fn [idx el]
                          (let [el (str el)]
                            (if (= ignore-el el) nil {:el-id el, :x idx}))))
           (remove nil?)))

(defn- new-antinode
  [{:keys [x-diff y-diff]} {x1 :x, y1 :y} {x2 :x, y2 :y}]
  {:x ((if (< x1 x2) - +) x1 x-diff), :y ((if (< y1 y2) - +) y1 y-diff)})

(defn antenas-two-antinodes
  "For a collection of two antenas, returns a collection of two antinodes that are closest to antenas"
  [[{x1 :x, y1 :y, :as cord-1} {x2 :x, y2 :y, :as cord-2}]]
  (let [diff {:x-diff (abs (- x1 x2)), :y-diff (abs (- y1 y2))}]
    [(new-antinode diff cord-1 cord-2) (new-antinode diff cord-2 cord-1)]))


(defn- all-antinodes
  [{:keys [max-x min-x max-y min-y]} diff cord1 cord2]
  (loop [last-antinode cord2
         acc [cord1]]
    (let [last-x (:x last-antinode)
          last-y (:y last-antinode)]
      (if
        (or (> last-x max-x) (< last-x min-x) (> last-y max-y) (< last-y min-y))
        acc
        (recur (new-antinode diff last-antinode (last acc))
               (conj acc last-antinode))))))

(defn cord-diff
  [{x1 :x, y1 :y} {x2 :x, y2 :y}]
  {:x-diff (abs (- x1 x2)), :y-diff (abs (- y1 y2))})

(defn antenas-all-antinodes
  "For a collection of two antenas, returns a collection all antinodes between :min-x, :max-x and :min-y, :max-y"
  [boundries [cord-1 cord-2]]
  (let [cord-1 (select-keys cord-1 [:x :y])
        cord-2 (select-keys cord-2 [:x :y])
        diff (cord-diff cord-1 cord-2)]
    (concat (all-antinodes boundries diff cord-1 cord-2)
            (all-antinodes boundries diff cord-2 cord-1))))


(defn find-nearest-unique-antinodes
  [city-plan]
  (let [max-x (-> city-plan
                  first
                  count
                  dec)
        max-y (-> city-plan
                  count
                  dec)
        antenas (->> max-y
                     inc
                     range
                     (reduce (fn [acc y]
                               (->> y
                                    city-plan
                                    row-els
                                    (map #(assoc % :y y))
                                    (group-by :el-id)
                                    (merge-with into acc)))
                       {})
                     vals)]
    (->> antenas
         (mapcat (comp #(mapcat antenas-two-antinodes %)
                       unordered-two-combinations))
         (filter (fn [{:keys [x y]}]
                   (and (>= x 0) (>= y 0) (<= x max-x) (<= y max-y))))
         (into #{})
         count)))

(defn find-all-unique-antinodes
  [city-plan]
  (let [max-x (-> city-plan
                  first
                  count
                  dec)
        max-y (-> city-plan
                  count
                  dec)
        boundries {:max-x max-x, :min-x 0, :max-y max-y, :min-y 0}
        antenas (->> max-y
                     inc
                     range
                     (reduce (fn [acc y]
                               (->> y
                                    city-plan
                                    row-els
                                    (map #(assoc % :y y))
                                    (group-by :el-id)
                                    (merge-with into acc)))
                       {})
                     vals)]
    (->> antenas
         (mapcat (comp #(mapcat (partial antenas-all-antinodes boundries) %)
                       unordered-two-combinations))
         (filter (fn [{:keys [x y]}]
                   (and (>= x 0) (>= y 0) (<= x max-x) (<= y max-y))))
         (into #{})
         count)))


(comment
  ;;{:x 2 :y 3} -> {:x 0 :y 1} -> {:x -2 :y -1}
  ;;{:x 4 :y 5} -> {:x 6 :y 7} ->
  ;;diff-x 2 diff-y 2
  ;;
  ;;
  ;; [a b c d] -> [(a,b) (a,c) (a,d) (b,c) (b,d) (c,d)]
  ;; How to find two antenas are aligned... All are aligned in some way...
  ;; It just don't makes sense to scan all of them for optimization. So we
  ;; need:
  ;; 1. Find all antenas of same type
  ;; 2. Create all ordered pairs of combinations of them
  ;; 3. For each pair find antinodes.
  ;; 4. Remove all antinodes that are outside of map
  ;; (btw we could drop this point if we only find antinodes for the ones
  ;; that will have them in a max-x and max-y, when we do the catesian
  ;; product to big, but we don't need to optimize prematurely
  ;;Part 1
  (-> antennas-map
      (str/split #"\n")
      find-nearest-unique-antinodes)
  (-> "src/kaspazza/2024/8/data.txt"
      slurp
      (str/split #"\n")
      find-nearest-unique-antinodes)
  ;;Part 2
  (-> antennas-map
      (str/split #"\n")
      find-all-unique-antinodes)
  (-> "src/kaspazza/2024/8/data.txt"
      slurp
      (str/split #"\n")
      find-all-unique-antinodes)
  ;
)
