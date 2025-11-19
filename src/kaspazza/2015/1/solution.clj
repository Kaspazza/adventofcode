(ns kaspazza.2015.1.solution)

(def examples
  [{:input "(())"
    :output 0}
   {:input "))((((("
    :output 3}
   {:input ")())())"
    :output -3}])


(defn count-parens
  [parens]
  (let [parens-count (-> identity
                         (group-by parens)
                         (update-vals count))
        opening-parens (get parens-count \()
        closing-parens (get parens-count \))]
    (- opening-parens closing-parens)))


(defn count-parens-better
  [parens]
  (let [parens-count (frequencies parens)
        opening-parens (get parens-count \()
        closing-parens (get parens-count \))]
    (- opening-parens closing-parens)))

(def basement -1)

(defn find-basement
  [parens]
  (reduce (fn [{:keys [floor position]
                :as acc}
               paren]
            (let [new-floor (if (= paren \() (inc floor) (dec floor))]
              (if (= new-floor basement)
                (reduced position)
                (-> acc
                    (assoc :floor new-floor)
                    (update :position inc)))))
          {:floor 0
           :position 1}
          parens))


(comment
  (count-parens (:input (get examples 2)))
  ;; Part 1
  (-> "src/kaspazza/2015/1/data.txt"
      slurp
      count-parens)
  ;; Part 2
  (-> "src/kaspazza/2015/1/data.txt"
      slurp
      find-basement))
