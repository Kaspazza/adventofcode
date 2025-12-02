(ns kaspazza.2025.1.solution
  (:require
   [clojure.string :as str]))

(def example "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
")

(def starting-dial 50)

(defn left? [direction] (= direction \L))

(defn right? [direction] (= direction \R))

(defn change-dial-left
  [current-dial turn-num]
  (if (>= current-dial turn-num) (- current-dial turn-num) (- (+ 100 current-dial) turn-num)))

(defn change-dial-right
  [current-dial turn-num]
  (if (> (+ current-dial turn-num) 99) (- (+ current-dial turn-num) 100) (+ current-dial turn-num)))

(defn calculate-current-dial
  [old-dial direction change]
  (let [turn-num (-> change
                     (mod 100))]
    (cond
      (left? direction) (change-dial-left old-dial turn-num)
      (right? direction) (change-dial-right old-dial turn-num))))

(defn over-zero-count
  [direction current-dial turn-num]
  (+
   (quot turn-num 100)
   (cond
     (and (not (zero? current-dial)) (left? direction) (< (- current-dial (mod turn-num 100)) 0)) 1
     (and (not (zero? current-dial)) (right? direction) (> (+ current-dial (mod turn-num 100)) 100))
     1
     :else 0)))

(defn calculate-zeros
  ([rotations] (calculate-zeros rotations false))
  ([rotations all-zeros?]
   (->> rotations
        (reduce (fn [{:keys [current-dial _zero-count]
                      :as acc}
                     change]
                  (let [turn-num (-> change
                                     (subs 1)
                                     parse-long)
                        direction (first change)
                        change-res (calculate-current-dial current-dial direction turn-num)
                        clicked-zero (over-zero-count direction current-dial turn-num)]
                    (cond-> acc
                      true (assoc :current-dial change-res)
                      (zero? change-res) (update :zero-count inc)
                      all-zeros? (update :zero-count #(+ % clicked-zero)))))
                {:current-dial starting-dial
                 :zero-count 0})
        :zero-count)))

(comment

 (-> example
     (str/split #"\n")
     calculate-zeros)

 (-> example
     (str/split #"\n")
     (calculate-zeros true))

 ;; Part 1
 (-> "src/kaspazza/2025/1/data.txt"
     slurp
     (str/split #"\n")
     calculate-zeros)

 ;; Part 2
 (-> "src/kaspazza/2025/1/data.txt"
     slurp
     (str/split #"\n")
     (calculate-zeros true))

)
