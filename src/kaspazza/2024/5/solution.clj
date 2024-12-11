(ns kaspazza.2024.5.solution
  (:require [clojure.string :as str]))

(def puzzle-input
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defn middle-el [v] (let [idx (quot (count v) 2)] (nth v idx)))

(defn parse-rules
  [rules]
  (->> (str/split rules #"\n")
       (map #(str/split % #"\|"))
       (reduce (fn [acc [frule srule :as rule]]
                 (update acc (str frule) conj (str srule)))
         {})))

(defn parse-orders
  [orders]
  (->> (str/split orders #"\n")
       (map #(str/split % #","))))

(defn valid-order?
  [rules order]
  (reduce (fn [acc el]
            (if-let [disallowed-els (get rules el)]
              (if (some (fn [el] (true? (some #(= % el) disallowed-els))) acc)
                (reduced false)
                (conj acc el))
              (conj acc el)))
    []
    order))

(defn- has-no-incoming?
  [node graph result]
  (not-any? #(contains? (set (get graph %)) node)
            (remove #(contains? (set result) %) (keys graph))))

(defn- find-node-with-no-incoming
  [graph result]
  (first (filter #(and (has-no-incoming? % graph result)
                       (not (contains? (set result) %)))
           (keys graph))))

(defn topological-sort
  [graph]
  (loop [result []
         remaining-nodes (count graph)]
    (if (zero? remaining-nodes)
      result
      (when-let [node (find-node-with-no-incoming graph result)]
        (recur (conj result node) (dec remaining-nodes))))))


(defn solution-part-1
  [s]
  (let [[rules orders] (-> s
                           (str/split #"\n\n"))]
    (->> orders
         parse-orders
         (filter (partial valid-order? (parse-rules rules)))
         (map (comp #(Integer/parseInt %) middle-el))
         (reduce +))))

(defn solution-part-2
  [s]
  (let [[rules orders] (-> s
                           (str/split #"\n\n"))
        rules (parse-rules rules)]
    (->> orders
         parse-orders
         (filter #(not (valid-order? rules %)))
         (map (fn [order]
                (->> order
                     (select-keys rules)
                     topological-sort)))
         (map (comp #(Integer/parseInt %) middle-el))
         (reduce +))))

(comment
  (require '[clojure.string :as str])
  ;;Part 1
  (solution-part-1 puzzle-input)
  (->> "src/kaspazza/2024/5/data.txt"
       slurp
       solution-part-1)
  ;;Part 2
  (solution-part-2 puzzle-input)
  (->> "src/kaspazza/2024/5/data.txt"
       slurp
       solution-part-2)
  ;
)
