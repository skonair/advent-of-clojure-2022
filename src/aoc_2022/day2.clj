(ns aoc-2022.day2
  (:require [clojure.string :as str]))

(defn- ascii-val [s]
  (int (first s)))

(defn- selection-score [[other my]
  (cond
    (= "X" my) 1
    (= "Y" my) 2
    (= "Z" my) 3))

(defn- win-score [[other my]
  (let [diff (- (ascii-val my) (ascii-val other) 23)]
    (cond
      (zero? diff) 3
      (= 1 (mod diff 3)) 6
      :otherwise 0)))

(defn- points [game]
  (+ (selection-score game) (win-score game)))

(defn part1 [games]
  (apply + (map points games)))

(defn- winning-card [card]
  (cond
    (= card "A") "Y"
    (= card "B") "Z"
    :otherwise "X"))

(defn- loosing-card [card]
  (cond
    (= card "A") "Z"
    (= card "B") "X"
    :otherwise "Y"))

(defn- equal-card [card]
  (cond
    (= card "A") "X"
    (= card "B") "Y"
    :otherwise "Z"))

(defn- guess-game [[other my]]
  (let [
    guessed-card (cond
      (= my "X") (loosing-card other)
      (= my "Z") (winning-card other)
      :otherwise (equal-card other))
      ]
      [other guessed-card]))

(defn part2 [games]
  (apply + (map points (map guess-game games))))

(def games (map #(str/split % #" ") (str/split-lines (slurp "resources/day2_input.txt"))))

(println "Day 2 - 1: " (part1 games)) ;15632
(println "Day 2 - 2: " (part2 games)) ;14416
