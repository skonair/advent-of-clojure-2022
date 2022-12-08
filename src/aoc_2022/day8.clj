(ns aoc-2022.day8
  (:require [clojure.string :as str]))


(defn- trees-from-top [[x y]]
  (map #(vector x %) (range y)))

(defn- trees-from-bottom [[x y] max-y]
  (map #(vector x %) (range max-y y -1)))

(defn- trees-from-left [[x y]]
  (map #(vector % y) (range x)))

(defn- trees-from-right [[x y] max-x]
  (map #(vector % y) (range max-x x -1)))

(defn- max-height [grid trees]
  (apply max (map #(Integer. (str (grid %))) trees)))

(defn- seen? [grid tree]
  (let [tree-height (Integer. (str (grid tree)))
        max-x (apply max (map (comp first first) grid))
        max-y (apply max (map (comp second first) grid))
        max-height-top (max-height grid (trees-from-top tree))
        max-height-bottom (max-height grid (trees-from-bottom tree max-y))
        max-height-left (max-height grid (trees-from-left tree))
        max-height-right (max-height grid (trees-from-right tree max-x))
  ]
  (or
    (> tree-height max-height-top)
    (> tree-height max-height-bottom)
    (> tree-height max-height-left)
    (> tree-height max-height-right))))


(defn- all-trees [pos max-x max-y]
    (concat (trees-from-top pos) (trees-from-bottom pos max-y) (trees-from-left pos) (trees-from-right pos max-x)))


(defn part1 [grid]
  (let [max-x (apply max (map (comp first first) grid))
        max-y (apply max (map (comp second first) grid))]
        (+
        (count (filter true? (map #(seen? grid %) (for [x (range 1 max-x) y (range 1 max-y)] [x y]))))
        max-x
        max-x
        max-y
        max-y)))

(defn part2 []
)

(def lines (str/split-lines (slurp "resources/day8_input.txt")))

(def grid (into {} (flatten (map-indexed (fn [y line] (map-indexed (fn [x tree] {[x y] tree}) line)) lines))))

(println "Day 8 - 1: " (part1 input))
(println "Day 8 - 2: " (part2 input))
