(ns kaspazza.2025.2.solution
  (:require
   [clojure.math   :as math]
   [clojure.string :as str]))

;; Part 1 - notes 
;; 1. It has to be even number of digits, xx, xxxx, xxxxxx.
;; 2. First half must be identical to second half, x | x, xx | xx, xxx | xxx <- so we need to find all variations where each of the numbers on both sides are the same
;; 3. We have a limit what those numbers could be, as we have a range, so if range is 30-60, we know at first place it can't be 1,2 and it can't be 7,8,9.
;;    So it leaves us with 3,4,5,6.
;;    Than at second place we can have any number 1-9, except if the first one is 6, than it can only be 0.
;;    But also we know both needs to be identical, so.. actually in that example of 2 numbers, it can be only 3,4,5 (as can;t go over 60).
;;
;;
;;   Let's check for example with four, xxxx, e.g. 3000-6000 range. At first place it can be only 3,4,5 as before, but on next one it can be 1-9, because 3939 is valid and a match.
;;   And third,fourth follow same rules as first and second. as we have xx | xx. so third can be only 3,4,5 as anything else would not make same two due to limit on the first half.
;;
;;   And what if we have not 000
;;   e.g.
;;   3000-5453
;;
;;   3131
;;   3232
;;   3333
;;   ...
;;   3939
;;   4040 <- next one needs to have 4 on both starting positions
;;   4141
;;   ...
;;   5353 <- in terms of the last one, we have a limit on next digit, so in this case 54 and 53  on other side, needs to take smallest  => 3 so 5353
;;   So we need to take (min x) of each position of each half after first digit
;;
;;
;;  1. 3,4,5 <- true
;;  2.
;;  3. 3,4,5
;;
;;    In case of 6 xxx|xxx  300000-600000
;;    first number same, only 3,4,5. and further numbers can be whatever, but after dividing it in half, the other half must
;;    599599
;;    
;; 4. There is also a case where range has variadic count, so imagine like 20-2000. But for the case of our data it's only 1 difference

;; 3567 - 5752

;; so the range is 3-5, 3,4,5

;; And we have in half so 31,32,33,34...50

;; So we need to up it by one.

;; And go (range 35 (inc 57))



;; Split even number in half. Get all possible values (range between 
;; 3000 - 5000
;; 3131
;; 3232
;; 3333
;; 3434
;;
;; Idea, in that first number range, 



(def example
  "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defn even-range
  [start end]
  (if (not= (- (count start) (count end)) 0)
    (if (odd? (count start))
      [(str (int (math/pow 10 (dec (count end))))) end]
      [start (str (int (- (math/pow 10 (count start)) 1)))])
    [start end]))

(defn invalid-ids
  [start end]
  (if (and (odd? (count start)) (odd? (count end)))
    [] ;; If it's odd range there will be no pair
    (let [[new-start new-end] (even-range start end)
          mid-idx (quot (count new-start) 2)
          r (range (parse-long (subs new-start 0 mid-idx))
                   (inc (inc (parse-long (subs new-end 0 mid-idx))))) ;; double inc, one for range internals being -1 and one for all the numbers that come after the half e.g. 5787, so we round it to 5800 basically
         ]
      (->> r
           (map (fn [n]
                  (let [num (parse-long (str n n))]
                    (when (and (>= num (parse-long start)) (<= num (parse-long end))) num))))
           (remove nil?)))))

(defn divisors [n] (filter #(zero? (mod n %)) (range 1 (inc (/ n 2)))))

(defn invalid-ids-part2
  [start end]
  (let [ids-range (range (parse-long start) (inc (parse-long end)))]
    (->> ids-range
         (map (fn [num]
                (let [strn (str num)
                      length (count strn)
                      partition-range (divisors length)]
                  (when (> length 1) ;; can't be a pattern if there is only one
                    (->> partition-range
                         (some (fn [pr] (when (apply = (partition pr strn)) strn))))))))
         (remove nil?))))

(comment
 (->>
   (str/split example #",")
   (map #(str/split % #"-"))
   (map #(apply invalid-ids %))
   flatten
   (apply +))

 (->> (-> "src/kaspazza/2025/2/data.txt"
          slurp
          (str/replace #"\r?\n" "")
          (str/split #",")
      )
      (map #(str/split % #"-"))
      (map #(apply invalid-ids %))
      flatten
      (apply +)
 )
 #_28146997880


 (->>
   (str/split example #",")
   (map #(str/split % #"-"))
   (map #(apply invalid-ids-part2 %))
   flatten
   (map parse-long)
   (apply +))
 #_4174379265

 (->> (-> "src/kaspazza/2025/2/data.txt"
          slurp
          (str/replace #"\r?\n" "")
          (str/split #","))
      (map #(str/split % #"-"))
      (map #(apply invalid-ids-part2 %))
      flatten
      (map parse-long)
      (apply +))

)

