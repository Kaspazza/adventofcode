(ns kaspazza.2024.9.solution
  (:require [clojure.string :as str]))

(def disk-map "2333133121414131402")

(def disk-map2 "90909")

(defn split-by-count
  [items target-sum]
  (loop [remaining items
         accumulated []
         current-sum 0]
    (if (empty? remaining)
      [accumulated []]
      (let [current-item (first remaining)
            item-count (:count current-item)
            new-sum (+ current-sum item-count)]
        (cond (= new-sum target-sum) [(conj accumulated current-item)
                                      (rest remaining)]
              (> new-sum target-sum)
                (let [remaining-needed (- target-sum current-sum)
                      split-item (assoc current-item :count remaining-needed)
                      remainder-item (assoc current-item
                                       :count (- item-count remaining-needed))
                      remaining-items (cons remainder-item (rest remaining))]
                  [(conj accumulated split-item) remaining-items])
              :else (recur (rest remaining)
                           (conj accumulated current-item)
                           new-sum))))))

(defn empty-spaces-blocks
  [file-blocks empty-spaces]
  (reduce (fn [{:keys [spaces blocks idx], :as acc} empty-space-count]
            (cond (= (- (count empty-spaces)
                        (- (count file-blocks) (count blocks)))
                     idx)
                    (reduced acc)
                  (= 0 empty-space-count) {:spaces (conj spaces {:count 0}),
                                           :blocks blocks,
                                           :idx (inc idx)}
                  :else (let [[new-spaces new-blocks]
                                (split-by-count blocks empty-space-count)]
                          {:spaces (conj spaces new-spaces),
                           :blocks new-blocks,
                           :idx (inc idx)})))
    {:spaces [], :blocks (reverse file-blocks), :idx 0}
    empty-spaces))

(defn filesystem
  [all-nums]
  (let [file-blocks (->> all-nums
                         (take-nth 2)
                         (map-indexed (fn [idx count]
                                        {:count count, :value idx})))
        empty-spaces (->> all-nums
                          rest
                          (take-nth 2))
        {:keys [spaces blocks]} (empty-spaces-blocks file-blocks empty-spaces)
        to-drop (- (count file-blocks) (count blocks))
        dropped (first blocks)]
    (concat (->> file-blocks
                 (drop-last (inc to-drop))
                 (mapcat (fn [empty-blocks {block-count :count, block-v :value}]
                           (if (= (:count empty-blocks) 0)
                             (repeat block-count block-v)
                             (concat (repeat block-count block-v)
                                     (mapcat (fn [{:keys [count value]}]
                                               (repeat count value))
                                       empty-blocks))))
                   spaces))
            (repeat (:count dropped) (:value dropped)))))

;;what needs to happen:
;;- Go over elements in blocks from last to first
;;- Find if there is in spaces el between idx of blocks to 0
;;- If there is not, go to next element in blocks
;;- If there is:
;;  If it's equal count
;;  Change it to count 0 in spaces
;;  If it's less
;;  Change it to count 0 and create new one with diff
;;
;;  And for blocks, move it to idx of spaces + 1
;;
;;9090909
;;
;;9 9 9 9
;; 0 0 0
;;
;;
;;

(defn find-match-idx
  [{:keys [count], :as block} blocks]
  (let [index (first (keep-indexed (fn [idx x] (when (= x block) idx)) blocks))]
    (->> index
         inc
         (range 0)
         (some (fn [idx]
                 (tap> [:idx idx :nth-block (nth blocks idx) :size count])
                 (let [{:keys [count id]} (nth blocks idx)]
                   (when (and (= id :empty) (>= count count)) idx)))))))

(defn- replace-index
  [blocks idx el]
  (let [[fhalf shalf] (split-at idx blocks)
        shalf (rest shalf)]
    (concat fhalf el shalf)))

(defn switch-blocks
  [idx1 idx2 blocks]
  (let [el1 (nth blocks idx1)
        el2 (nth blocks idx2)]
    (tap> [:switch-blocks blocks :el1 el1 :el2 el2 :res
           (cond (= (:count el1) (:count el2)) (-> blocks
                                                   (assoc idx1 el2)
                                                   (assoc idx2 el1))
                 (> (:count el1) (:count el2))
                   (let [diff (- (:count el1) (:count el2))
                         new-el [el2 {:count diff, :value 0, :id :empty}]]
                     (-> blocks
                         (assoc-in [idx2 :value] 0)
                         (assoc-in [idx2 :id] :empty)
                         (replace-index idx1 new-el)
                         vec))
                 :else blocks)])
    (cond (= (:count el1) (:count el2)) (-> blocks
                                            (assoc idx1 el2)
                                            (assoc idx2 el1))
          (> (:count el1) (:count el2))
            (let [diff (- (:count el1) (:count el2))
                  new-el [el2 {:count diff, :value 0, :id :empty}]]
              (-> blocks
                  (assoc-in [idx2 :value] 0)
                  (assoc-in [idx2 :id] :empty)
                  (replace-index idx1 new-el)
                  vec))
          :else blocks)))

(defn minimize-empty-blocks
  [all-blocks]
  (->> all-blocks
       reverse
       (reduce (fn [acc block]
                 (tap> [:el block])
                 (if (= :file (:id block))
                   (if-let [match-idx (find-match-idx block (:blocks acc))]
                     (-> acc
                         (update :blocks
                                 #(switch-blocks match-idx (:idx acc) %))
                         (update :idx dec))
                     (update acc :idx dec))
                   (update acc :idx dec)))
         {:blocks all-blocks, :idx (dec (count all-blocks))})
       :blocks))

(defn filesystem2
  [all-nums]
  (let [file-blocks (->> all-nums
                         (take-nth 2)
                         (map-indexed (fn [idx count]
                                        {:count count, :value idx, :id :file})))
        empty-spaces (->> all-nums
                          rest
                          (take-nth 2)
                          (map (fn [count]
                                 {:count count, :value 0, :id :empty})))
        all-blocks (->
                     (mapcat (fn [el1 el2] [el1 el2]) file-blocks empty-spaces)
                     vec
                     (conj (last file-blocks)))]
    (->> all-blocks
         minimize-empty-blocks
         (mapcat #(repeat (:count %) (:value %))))))



(comment
  ;;Idea: If we filter out every second and count all numbers we know how
  ;;many we have.
  ;;     And then we could just go in blank spaces (filter every second
  ;;     going from (rest..)) and reduce the right side untill there is
  ;;     none.
  ;;
  ;;So each empty-space, tells us how many file-blocks we should move. We
  ;;always have 1to1, so for each file-block we have empty-block on the
  ;;idx-1
  ;;
  ;;
  ;;We need to go for the file-blocks from the end, and stop at the moment
  ;;when sum is equal to empty-spaces-places ! remember to account for the
  ;;last one, that could be in-between (e.g.
  ;;part stays at the end, part at top)
  ;;
  ;;
  ;;Idea: we could also make it more explicit, by going over all
  ;;file-blocks and assigning them their value and amount. This will be
  ;;less performant, but it will make it much more clear and easier to
  ;;debug/change.
  ;;
  ;; Part 1
  (->> (str/split disk-map #"")
       (map parse-long)
       filesystem
       (map-indexed (fn [idx el] (* idx el)))
       (reduce +))
  (let [input (-> "src/kaspazza/2024/9/data.txt"
                  slurp
                  (str/split #"")
                  drop-last)]
    (->> input
         (map parse-long)
         filesystem
         (map-indexed (fn [idx el] (* idx el)))
         (reduce +)))
  ;;Part 2
  (->> (str/split disk-map #"")
       (map parse-long)
       filesystem2
       (map-indexed (fn [idx el] (* idx el)))
       (reduce +))
  ;;
  (let [input (-> "src/kaspazza/2024/9/data.txt"
                  slurp
                  (str/split #"")
                  drop-last)]
    (->> input
         (map parse-long)
         filesystem
         (map-indexed (fn [idx el] (* idx el)))
         (reduce +)))
  ;
)
