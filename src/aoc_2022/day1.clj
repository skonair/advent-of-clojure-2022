(ns aoc-2022.day1
  (:require [clojure.string :as str]))


(defn add-up [calory-lines]
  (apply + (map #(Integer. %) calory-lines)))

(defn part1 [calories]
  (apply max (map add-up calories)))

(defn part2 [calories]
  (apply + (take-last 3 (sort (map add-up calories))))

(def lines (str/split-lines (slurp "resources/day1_input.txt")))
(def input (filter #(not= '("") %) (partition-by empty? lines)))

(println "Day 1 - 1: " (part1 input)) ; 72070
(println "Day 1 - 2: " (part2 input)) ; 211805

