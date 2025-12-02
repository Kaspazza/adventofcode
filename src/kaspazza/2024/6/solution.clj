(ns kaspazza.2024.6.solution
  (:require
   [clojure.set    :as set]
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

(def guard-patterns
  {"^" :up
   "v" :down
   ">" :right
   "<" :left})

(def obstacle "#")

(def empty-point ".")

(defn map-el-type
  [el]
  (cond
    (= el obstacle) :obstacle
    (= el empty-point) :empty
    (some #(= el %) (keys guard-patterns)) :guard
    :else :unknown))

(defn mark-elements
  [m]
  (map-indexed (fn [row-idx row]
                 (map-indexed (fn [col-idx el]
                                {:x col-idx
                                 :y row-idx
                                 :el el
                                 :el-type (map-el-type el)})
                              row))
               m))

(defn guard-up
  [obstacles
   {guard-x :x
    guard-y :y
    :as _guard}]
  (if-let [{:keys [x y]} (some->> obstacles
                                  (filter (fn [{:keys [x y]
                                                :as _obstacle}]
                                            (and (= x guard-x) (< y guard-y))))
                                  (sort-by :y)
                                  last)]
    {:visited-positions (into #{}
                              (map (fn [num]
                                     {:x x
                                      :y num})
                                   (range (inc y) (inc guard-y))))
     :guard-pos {:y (inc y)
                 :x x
                 :direction :right}}
    {:guard-pos nil
     :visited-positions (into #{}
                              (map (fn [num]
                                     {:x guard-x
                                      :y num})
                                   (range 0 guard-y)))}))

(defn guard-right
  [obstacles
   max-x
   {guard-x :x
    guard-y :y
    :as _guard}]
  (if-let [{:keys [x y]} (some->> obstacles
                                  (filter (fn [{:keys [x y]
                                                :as _obstacle}]
                                            (and (> x guard-x) (= y guard-y))))
                                  (sort-by :x)
                                  first)]
    {:visited-positions (into #{}
                              (map (fn [num]
                                     {:x num
                                      :y y})
                                   (range guard-x (dec x))))
     :guard-pos {:y y
                 :x (dec x)
                 :direction :down}}
    {:guard-pos nil
     :visited-positions (into #{}
                              (map (fn [num]
                                     {:x num
                                      :y guard-y})
                                   (range guard-x max-x)))}))

(defn guard-down
  [obstacles
   max-y
   {guard-x :x
    guard-y :y
    :as _guard}]
  (if-let [{:keys [x y]} (some->> obstacles
                                  (filter (fn [{:keys [x y]
                                                :as _obstacle}]
                                            (and (= x guard-x) (> y guard-y))))
                                  (sort-by :y)
                                  first)]
    {:visited-positions (into #{}
                              (map (fn [num]
                                     {:x x
                                      :y num})
                                   (range guard-y y)))
     :guard-pos {:y (dec y)
                 :x x
                 :direction :left}}
    {:guard-pos nil
     :visited-positions (into #{}
                              (map (fn [num]
                                     {:x guard-x
                                      :y num})
                                   (range guard-y (inc max-y))))}))

(defn guard-left
  [obstacles
   {guard-x :x
    guard-y :y
    :as _guard}]
  (if-let [{:keys [x y]} (some->> obstacles
                                  (filter (fn [{:keys [x y]
                                                :as _obstacle}]
                                            (and (< x guard-x) (= y guard-y))))
                                  (sort-by :x)
                                  last)]
    {:visited-positions (into #{}
                              (map (fn [num]
                                     {:x num
                                      :y y})
                                   (range (inc x) (inc guard-x))))
     :guard-pos {:y y
                 :x (inc x)
                 :direction :up}}
    {:guard-pos nil
     :visited-positions (into #{}
                              (map (fn [num]
                                     {:x num
                                      :y guard-y})
                                   (range 0 guard-x)))}))


(defn visited-positions
  [{:keys [max-x max-y]} obstacles guard]
  (loop [g guard
         visited-pos #{}]
    (let [current-g (last g)]
      (if (some #(= % current-g) (butlast g))
        {:guard-pos current-g
         :loop? true
         :visited-positions visited-pos}
        (if-let [guard-direction (:direction current-g)]
          (let [{:keys [guard-pos visited-positions]} (case guard-direction
                                                        :up (guard-up obstacles current-g)
                                                        :down (guard-down obstacles max-y current-g)
                                                        :right
                                                        (guard-right obstacles max-x current-g)
                                                        :left (guard-left obstacles current-g)
                                                        {:guard-pos nil
                                                         :visited-positions #{}})]
            (recur (conj g guard-pos) (set/union visited-pos visited-positions)))
          {:guard-pos g
           :loop? false
           :visited-positions visited-pos})))))

(let [puzzle-formated (-> (slurp "data.txt")
                          (str/split #"\n"))
      {:keys [obstacle guard]} (->> puzzle-formated
                                    (map #(str/split % #""))
                                    mark-elements
                                    (apply concat)
                                    (group-by :el-type))
      guard (first guard)
      max-x (dec (count (first puzzle-formated)))
      max-y (dec (count puzzle-formated))
      guard-initial [{:x (:x guard)
                      :y (:y guard)
                      :direction (get guard-patterns (:el guard))}]
      guard-positions (-> (visited-positions {:max-x max-x
                                              :max-y max-y}
                                             obstacle
                                             guard-initial)
                          :visited-positions
                          (set/difference #{{:x (:x guard)
                                             :y (:y guard)}}))]
  (->> guard-positions
       (map #(conj obstacle %))
       (map #(visited-positions {:max-x max-x
                                 :max-y max-y}
                                %
                                guard-initial))
       (filter :loop?)
       count))



(comment
  ;; Part 1
  (let [puzzle-formated (-> puzzle-input
                            (str/split #"\n"))
        {:keys [obstacle guard]} (->> puzzle-formated
                                      (map #(str/split % #""))
                                      mark-elements
                                      (apply concat)
                                      (group-by :el-type))
        guard (first guard)
        guard-initial [{:x (:x guard)
                        :y (:y guard)
                        :direction (get guard-patterns (:el guard))}]
        max-x (dec (count (first puzzle-formated)))
        max-y (dec (count puzzle-formated))]
    (->> (visited-positions {:max-x max-x
                             :max-y max-y}
                            obstacle
                            guard-initial)
         :visited-positions
         count))
  (let [puzzle-formated (-> (slurp "src/kaspazza/2024/6/data.txt")
                            (str/split #"\n"))
        {:keys [obstacle guard]} (->> puzzle-formated
                                      (map #(str/split % #""))
                                      mark-elements
                                      (apply concat)
                                      (group-by :el-type))
        guard (first guard)
        guard-initial [{:x (:x guard)
                        :y (:y guard)
                        :direction (get guard-patterns (:el guard))}]
        max-x (dec (count (first puzzle-formated)))
        max-y (dec (count puzzle-formated))]
    (->> (visited-positions {:max-x max-x
                             :max-y max-y}
                            obstacle
                            guard-initial)
         :visited-positions
         count))
  ;; Part 2
  ;; Basically we are looking for a moment where after 4 moves, guard position is the same... Hmm no, it could be more than 4...
  ;; So basically we are looking for a moment where after X moves, guard position is the same
  ;; So one thing is about finding them at all.
  ;; But simple first step would be how to notice we are in a one. So we need to basicaly have loop finding.
  ;; Simple solution I see rn is to keep all past guard positions and if new one matches it we found a loop.
  ;; Then it is to add one new obstacle on all of the visited ones and search through them to count looping ones.
  (let [puzzle-formated (-> puzzle-input
                            (str/split #"\n"))
        {:keys [obstacle guard]} (->> puzzle-formated
                                      (map #(str/split % #""))
                                      mark-elements
                                      (apply concat)
                                      (group-by :el-type))
        guard (first guard)
        max-x (dec (count (first puzzle-formated)))
        max-y (dec (count puzzle-formated))
        guard-initial [{:x (:x guard)
                        :y (:y guard)
                        :direction (get guard-patterns (:el guard))}]
        guard-positions (-> (visited-positions {:max-x max-x
                                                :max-y max-y}
                                               obstacle
                                               guard-initial)
                            :visited-positions
                            (set/difference #{{:x (:x guard)
                                               :y (:y guard)}}))]
    (->> guard-positions
         (map #(conj obstacle %))
         (map #(visited-positions {:max-x max-x
                                   :max-y max-y}
                                  %
                                  guard-initial))
         (filter :loop?)
         count))
  (let [puzzle-formated (-> (slurp "src/kaspazza/2024/6/data.txt")
                            (str/split #"\n"))
        {:keys [obstacle guard]} (->> puzzle-formated
                                      (map #(str/split % #""))
                                      mark-elements
                                      (apply concat)
                                      (group-by :el-type))
        guard (first guard)
        max-x (dec (count (first puzzle-formated)))
        max-y (dec (count puzzle-formated))
        guard-initial [{:x (:x guard)
                        :y (:y guard)
                        :direction (get guard-patterns (:el guard))}]
        guard-positions (-> (visited-positions {:max-x max-x
                                                :max-y max-y}
                                               obstacle
                                               guard-initial)
                            :visited-positions
                            (set/difference #{{:x (:x guard)
                                               :y (:y guard)}}))]
    (->> guard-positions
         (map #(conj obstacle %))
         (map #(visited-positions {:max-x max-x
                                   :max-y max-y}
                                  %
                                  guard-initial))
         (filter :loop?)
         count)))
