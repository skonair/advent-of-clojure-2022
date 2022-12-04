(ns aoc-2022.day4
  (:require [clojure.string :as str]))

(defn- get-ranges [line]
  (map #(Integer. %) (str/split line #"[,\-]")))

(defn- contains-full? [[s1 e1 s2 e2]]
  (or
    (and (<= s1 s2) (>= e1 e2))
    (and (>= s1 s2) (<= e1 e2))))

(defn part1 [lines]
  (count 
    (filter 
      true? 
      (map 
        contains-full? 
        (map get-ranges lines)))))

(defn- overlaps? [[s1 e1 s2 e2]]
  (not 
    (empty? 
      (set/intersection (into #{} (range s1 (inc e1))) 
                        (into #{} (range s2 (inc e2)))))))

(defn part2 [lines]
  (count 
    (filter 
      true? 
      (map 
        overlaps? 
        (map get-ranges lines)))))

(def lines (str/split-lines (slurp "resources/day4_input.txt")))

(println "Day 4 - 1: " (part1 lines)) ;569
(println "Day 4 - 2: " (part2 lines)) ;936
