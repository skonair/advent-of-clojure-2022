(ns aoc-2022.day3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn- split-line [line]
  (split-at (quot (count line) 2) line))

(defn- common-itemtype [lines]
  (first
     (apply 
       set/intersection 
       (map #(into #{} %) lines))))

(defn- priority [itemtype]
  (let [ascii-val (int itemtype)]
    (if 
      (> ascii-val 96) 
      (- ascii-val 96)
      (- ascii-val 38))))

(defn part1 [lines]
  (apply 
    + 
    (map 
      #(priority (common-itemtype (split-line %))) 
      lines)))

(defn part2 [lines]
  (apply 
    + 
    (map 
      #(priority (common-itemtype %)) 
      (partition 3 lines))))

(def lines (str/split-lines (slurp "resources/day3_input.txt")))

(println "Day 3 - 1: " (part1 lines)) ;8202
(println "Day 3 - 2: " (part2 lines)) ;2864
