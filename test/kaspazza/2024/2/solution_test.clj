(ns kaspazza.2024.2.solution-test
  (:require [clojure.test :refer [deftest is]]
            [kaspazza.2024.2.solution :as sut]))

(deftest safe-report
  (is (true? (sut/report-safe? [7 6 4 2 1]))
      "Safe because the levels are all decreasing by 1 or 2.")
  (is (true? (sut/report-safe? [1 3 6 7 9]))
      "Safe because the levels are all increasing by 1,2 or 3.")
  (is (false? (sut/report-safe? [1 2 7 8 9]))
      "Unsafe because 2 7 is an increase of 5")
  (is (false? (sut/report-safe? [9 7 6 2 1]))
      "Unsafe because 6 2 is a decrease of 4.")
  (is (false? (sut/report-safe? [8 6 4 4 1]))
      "Unsafe because 4 4 is neither an increase or a decrease.")
  (is (false? (sut/report-safe? [1 2 7 8 9]))))

(deftest dampener-report-safe
  (is (true? (sut/dampener-report-safe? [64 61 65 67 69]))))
