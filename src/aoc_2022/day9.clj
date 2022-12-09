(ns aoc-2022.day9
  (:require [clojure.string :as str]))

(defn- move-head [[hx hy] motion]
  (cond
    (= \R motion) [(inc hx) hy]
    (= \L motion) [(dec hx) hy]
    (= \U motion) [hx (inc hy)]
    (= \D motion) [hx (dec hy)]))

(defn- move-tail [[hx hy] [tx ty :as t]]
  (if
    (and (<= (Math/abs (- hx tx)) 1) (<= (Math/abs (- hy ty)) 1)) t
    (let [d [(compare hx tx) (compare hy ty)]]
      (mapv + t d))))

(defn- move [hpos tpos motion]
  (let [nhpos (move-head hpos motion)
        ntpos (move-tail nhpos tpos)]
    [nhpos ntpos]))

(defn part1 [motions]
  (loop [[m & ms] motions
         hpos [0 0]
         tpos [0 0]
         akk #{}]
    (if (nil? m)
      (count akk)
      (let [[nhpos ntpos] (move hpos tpos m)]
        (recur ms nhpos ntpos (conj akk ntpos))))))

; that's a stupid one :D
(defn part2 [motions]
  (loop [[m & ms] motions
         hpos0 [0 0]
         hpos1 [0 0]
         hpos2 [0 0]
         hpos3 [0 0]
         hpos4 [0 0]
         hpos5 [0 0]
         hpos6 [0 0]
         hpos7 [0 0]
         hpos8 [0 0]
         tpos [0 0]
         akk #{}]
    (if (nil? m)
      (count akk)
      (let [[nhpos0 nhpos1] (move hpos0 hpos1 m)
            nhpos2 (move-tail nhpos1 hpos2)
            nhpos3 (move-tail nhpos2 hpos3)
            nhpos4 (move-tail nhpos3 hpos4)
            nhpos5 (move-tail nhpos4 hpos5)
            nhpos6 (move-tail nhpos5 hpos6)
            nhpos7 (move-tail nhpos6 hpos7)
            nhpos8 (move-tail nhpos7 hpos8)
            ntpos (move-tail nhpos8 tpos)]
        (recur ms nhpos0 nhpos1 nhpos2 nhpos3 nhpos4 nhpos5 nhpos6 nhpos7 nhpos8 ntpos (conj akk ntpos))))))

(defn- parse-motion [line]
  (let [direction (first line)
        steps (Integer. (str (subs line 2)))]
    (repeat steps direction)))

(def motions (flatten (map parse-motion (str/split-lines (slurp "resources/day9_input.txt")))))

(println "Day 9 - 1: " (part1 input)) ; 6337
(println "Day 9 - 2: " (part2 input)) ; 2455
