(ns aoc-2022.day5
  (:require [clojure.string :as str]))

(defn- apply-move [stack [amount from to] stack-fct]
  (let [changes (take amount (stack from))
        new-from (drop amount (stack from))
        new-to (concat (stack-fct changes) (stack to))]
   (assoc (assoc stack to new-to) from new-from)))

(defn- apply-moves [stack moves stack-fct]
  (loop [s stack
         [m & ms] moves]
    (if (nil? m)
      s
      (recur (apply-move s m stack-fct) ms))))

(defn- solve [stack moves stack-fct]
  (apply
    str
    (map
      #(first (second %))
      (sort-by
        first
        (apply-moves stack moves stack-fct)))))

(defn part1 [stack moves]
  (solve stack moves reverse))

(defn part2 [stack moves]
  (solve stack moves identity))

(defn- parse-move [line]
  (map
    #(Integer. %)
    (take-nth
      2
      (drop
        1
        (str/split line #" ")))))

(def lines (str/split-lines (slurp "resources/day5_input.txt")))
(def moves (map parse-move lines))

;      [D]
;  [N] [C]
;  [Z] [M] [P]
;   1   2   3
(def test-input {1 "NZ" 2 "DCM" 3 "P"})

; is this cheating? :thinking:
(def input {1 "RHMPZ" 2 "BJCP" 3 "DCLGHNS" 4 "LRSQDMTF" 5 "MZTBQPSF" 6 "GBZSFT" 7 "VRN" 8 "MCVDTLGP" 9 "LMFJNQW"})

; solution 1
(apply str (map #(first (second %)) (sort-by first (part1 input moves))))

(println "Day 5 - 1: " (part1 input moves)) ; VQZNJMWTR
(println "Day 5 - 2: " (part2 input moves)) ; NLCDCLVMQ
