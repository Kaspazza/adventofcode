(ns kaspazza.2025.3.solution
  (:require
   [clojure.string :as str]))

(def example "987654321111111
811111111111119
234234234234278
818181911112111")


(defn highest-joltage
  [bank]
  (let [max-num (count bank)]
    (reduce (fn [{:keys [highest-num second-highest curr]
                  :as acc}
                 j]
              (let [new-num (parse-long (str j))
                    new-acc (if (and (< curr max-num) ;; When we are at last position we can't assing first num as we won't have second :)
                                     (> new-num highest-num))
                              (-> acc
                                  (assoc :highest-num new-num)
                                  (assoc :second-highest -1))
                              (if (> new-num second-highest)
                                (-> acc
                                    (assoc :second-highest new-num))
                                acc))]
                (update new-acc :curr inc)))
            {:highest-num -1
             :second-highest -1
             :curr 1}
            bank)))

(defn joltage-sum
  [banks]
  (->> banks
       (transduce (comp (map highest-joltage)
                        (map (juxt :highest-num :second-highest))
                        (map #(apply str %))
                        (map parse-long))
                  +)))

;;Part 2

(defn char->int [c] (parse-long (str c)))

(defn digit-idx-within-range?
  [all-digits max-digits digit-idx]
  (<= digit-idx (- all-digits max-digits)))

(defn replace-digit?
  [all-digits max-digits curr-digit-idx current-digit highest-num]
  (and (digit-idx-within-range? all-digits max-digits curr-digit-idx)
       (> current-digit highest-num)))

(defn pad-to-12
  [v]
  (let [missing (- 12 (count v))] (if (pos? missing) (into v (repeat missing -1)) v)))

(defn highest-joltage2
  [bank]
  (let [max-digits 12
        all-digits (count bank)]
    (reduce (fn [{:keys [biggest-num curr-digit-idx]} curr-digit-char]
              (let [current-digit (char->int curr-digit-char)
                    new-biggest-num
                    (reduce
                     (fn [{:keys [idx highest-num]} idx-value]
                       (if (and
                            (digit-idx-within-range? all-digits (- max-digits idx) curr-digit-idx)
                            (> current-digit idx-value))
                         (reduced {:highest-num (pad-to-12 (conj highest-num current-digit))})
                         {:idx (inc idx)
                          :highest-num (conj highest-num idx-value)}))
                     {:idx 1
                      :highest-num []}
                     biggest-num)]
                {:biggest-num (:highest-num new-biggest-num)
                 :curr-digit-idx (inc curr-digit-idx)}))
            {:biggest-num (vec (repeat 12 -1))
             :curr-digit-idx 1}
            bank)))

;; Problem find highest 12 digit number
;; First - needs to find highest number in all nums that can be 12 digits.
;; Second - in the range between this number index and end of numbers need to find second biggest between this index and up to 11 numbers from end...
;; And so on untill we have 12 digits.




(comment

 (-> example
     (str/split #"\n")
     second
     highest-joltage2)

 (-> example
     (str/split #"\n")
     joltage-sum)
 #_357

 (-> "src/kaspazza/2025/3/data.txt"
     slurp
     (str/split #"\n")
     joltage-sum
 )
 #_17359

 (->> (-> example
          (str/split #"\n"))
      (map highest-joltage2)
      (map :biggest-num)
      (map #(apply str %))
      (map parse-long)
      (apply +)
 )
 #_3121910778619

 (->> (-> "src/kaspazza/2025/3/data.txt"
          slurp
          (str/split #"\n"))
      (map highest-joltage2)
      (map :biggest-num)
      (map #(apply str %))
      (map parse-long)
      (apply +))

)
