(ns aoc-2022.day6
  (:require [clojure.string :as str]))

(defn- contains-duplicate? [word]
  (not= (count word) (count (into #{} word))))

(defn- compute-marker [length input]
  (+
    length
    (second
      (first
        (filter
          (fn [[b n]] (not b))
          (map-indexed
            (fn [idx elem] [(contains-duplicate? elem) idx])
            (partition length 1 input)))))))

(defn part1 [input]
  (compute-marker 4 input))

(defn part2 [input]
  (compute-marker 14 input))

(def input (slurp "resources/day6_input.txt"))

(println "Day 6 - 1: " (part1 input)) ; 1042
(println "Day 6 - 2: " (part2 input)) ; 2980
