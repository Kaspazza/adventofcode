(ns kaspazza.2024.7.solution
  (:require
   [clojure.string :as str]))

(def puzzle-input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

  ;;There is also clojure.math.combinatorics which has cartesian-product implemented. Although for this ex this is fine
(defn cartesian-product [sets]
  (loop [acc [[]]
         rem sets]
    (if (empty? rem)
      acc
      (recur (mapcat (fn [a]
                       (map #(conj a %) (first rem)))
                     acc)
             (rest rem)))))

(defn parse-equation [eq]
  (->> eq
       (re-seq #"\d+")
       (map parse-long)))

(defn solve-equation [ops nums]
  (loop [ops ops
         acc (first nums)
         nums (rest nums)]
    (if (empty? ops)
      acc
      (recur (rest ops) ((first ops) acc (first nums)) (rest nums)))))

(defn is-any-equal-sum? [operations sum nums]
  (let [n (-> nums
              count
              dec)
        all-operation-options  (->> operations
                                    (repeat n)
                                    cartesian-product)]

    (some
     #(= sum (solve-equation % nums))
     all-operation-options)))

(defn || [n1 n2]
  (parse-long (str n1 n2)))

(comment
  ;;Part 1
  (let [equations (-> puzzle-input
                      (str/split #"\n"))
        eqs (map parse-equation equations)]
    (reduce
     (fn [acc x] (if (is-any-equal-sum? [+ *] (first x) (rest x))
                   (+ acc (first x))
                   acc))
     0
     eqs))
  (let [equations (-> (slurp "src/kaspazza/2024/7/data.txt")
                      (str/split #"\n"))
        eqs (map parse-equation equations)]
    (reduce
     (fn [acc x] (if (is-any-equal-sum? [+ *] (first x) (rest x))
                   (+ acc (first x))
                   acc))
     0
     eqs))
  ;;Part2
   (let [equations (-> puzzle-input
                      (str/split #"\n"))
        eqs (map parse-equation equations)]
    (reduce
     (fn [acc x] (if (is-any-equal-sum? [+ * ||] (first x) (rest x))
                   (+ acc (first x))
                   acc))
     0
     eqs))
   (let [equations (-> (slurp "src/kaspazza/2024/7/data.txt")
                       (str/split #"\n"))
        eqs (map parse-equation equations)]
    (reduce
     (fn [acc x] (if (is-any-equal-sum? [+ * ||] (first x) (rest x))
                   (+ acc (first x))
                   acc))
     0
     eqs))

;; So basically what we have here is combinatorics. 
;; We have N-1 places for operations where N is count of numbers (e.g. with 2 numbers it's 1 place, with 3 it's 2, with 4 it's 3)
;; In each of those places we have 2 options for operations (+, *)
;; So it will be 2^N-1

;;   10 19 (2^1=2) 
;;   => + 10 19
;;   => * 10 19

;;   10 19 25 (2^2=4)
;;   => (+ 10 (+ 19 25)
;;   => 10 + 19 * 25
;;   => 10 * 19 + 25
;;   => 10 * 19 * 25

;;   10 19 25 5 (2^3=8)
;;   => 10 + 19 + 25 + 5 
;;   => 10 + 19 + 25 * 5
;;   => 10 + 19 * 25 + 5
;;   => 10 * 19 + 25 + 5
;;   => 10 + 19 * 25 * 5
;;   => 10 * 19 * 25 + 5 
;;   => 10 * 19 + 25 * 5
;;   => 10 * 19 * 25 * 5

  )
