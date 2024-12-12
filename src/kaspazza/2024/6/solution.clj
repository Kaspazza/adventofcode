(ns kaspazza.2024.6.solution
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def puzzle-input
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

;; There are 4 states of GUARD, [^, v, >, <]
;; There are OBSTRUCTIONS # which if it's next should rotate 90 the guard
;; It would be good to replace each place guard is with X
;; It stops when guard as next element leaves a box
;;

(def guard-patterns {"^" :up, "v" :down, ">" :right, "<" :left})

(def obstacle "#")

(def empty-point ".")

(defn map-el-type
  [el]
  (cond (= el obstacle) :obstacle
        (= el empty-point) :empty
        (some #(= el %) (keys guard-patterns)) :guard
        :else :unknown))

(defn mark-elements
  [m]
  (map-indexed
    (fn [row-idx row]
      (map-indexed
        (fn [col-idx el]
          {:x col-idx, :y row-idx, :el el, :el-type (map-el-type el)})
        row))
    m))

(defn guard-up
  [obstacles {guard-x :x, guard-y :y, :as _guard}]
  (if-let [{:keys [x y]} (some->> obstacles
                                  (filter (fn [{:keys [x y], :as _obstacle}]
                                            (and (= x guard-x) (< y guard-y))))
                                  (sort-by :y)
                                  last)]
    {:visited-positions
       (into #{} (map (fn [num] {:x x, :y num}) (range (inc y) (inc guard-y)))),
     :guard-pos {:y (inc y), :x x, :direction :right}}
    {:guard-pos nil,
     :visited-positions
       (into #{} (map (fn [num] {:x guard-x, :y num}) (range 0 guard-y)))}))

(defn guard-right
  [obstacles max-x {guard-x :x, guard-y :y, :as _guard}]
  (if-let [{:keys [x y]} (some->> obstacles
                                  (filter (fn [{:keys [x y], :as _obstacle}]
                                            (and (> x guard-x) (= y guard-y))))
                                  (sort-by :x)
                                  first)]
    {:visited-positions
     (into #{} (map (fn [num] {:x num, :y y}) (range guard-x (inc x)))),
     :guard-pos {:y y, :x (inc x), :direction :down}}
    {:guard-pos nil,
     :visited-positions
     (into #{} (map (fn [num] {:x num, :y guard-y}) (range guard-x max-x)))}))

(defn guard-down
  [obstacles max-y {guard-x :x, guard-y :y, :as _guard}]
  (if-let [{:keys [x y]} (some->> obstacles
                                  (filter (fn [{:keys [x y], :as _obstacle}]
                                            (and (= x guard-x) (> y guard-y))))
                                  (sort-by :y)
                                  first)]
    {:visited-positions
     (into #{} (map (fn [num] {:x x, :y num}) (range guard-y (inc y)))),
     :guard-pos {:y (inc y), :x x, :direction :right}}
    {:guard-pos nil,
     :visited-positions
     (into #{} (map (fn [num] {:x guard-x, :y num}) (range guard-y (inc max-y))))}))




(defn guard-left
  [obstacles {guard-x :x, guard-y :y, :as _guard}]
  (if-let [{:keys [x y]} (some->> obstacles
                                  (filter (fn [{:keys [x y], :as _obstacle}]
                                            (and (> x guard-x) (= y guard-y))))
                                  (sort-by :x)
                                  first)]
    {:visited-positions
     (into #{} (map (fn [num] {:x num, :y y}) (range (inc x) (inc guard-x)))),
     :guard-pos {:y y, :x (inc x), :direction :up}}
    {:guard-pos nil,
     :visited-positions
     (into #{} (map (fn [num] {:x num, :y guard-y}) (range 0 guard-x)))}))



(defn visited-positions
  [{:keys [max-x max-y]} obstacles guard]
  (loop [g guard
         visited-pos #{}]
    (if-let [guard-direction (or (:direction g) (get guard-patterns (:el g)))]
      (let [{:keys [guard-pos visited-positions]} (case guard-direction
                                                    :up (guard-up obstacles g)
                                                    :down (guard-down  obstacles max-y g)
                                                    :right (guard-right obstacles max-x  g)
                                                    :left (guard-left obstacles g)
                                                    {:guard-pos nil,
                                                     :visited-positions #{}})]
        (recur guard-pos (set/union visited-pos visited-positions)))
      visited-pos)))

(comment
  (range 1 6)
  (let [puzzle-formated (-> puzzle-input
                            (str/split #"\n"))
        {:keys [obstacle guard]} (->> puzzle-formated
                                      (map #(str/split % #""))
                                      mark-elements
                                      (apply concat)
                                      (group-by :el-type))
        max-x (dec (count (first puzzle-formated)))
        max-y (dec (count puzzle-formated))]
    (visited-positions {:max-x max-x :max-y max-y} obstacle (first guard)))
  
  (count "abc")
  ;;
)
