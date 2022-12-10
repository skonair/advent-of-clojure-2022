(ns aoc-2022.day10
  (:require [clojure.string :as str]))

(defn- execute-addx [[instr steps arg] reg]
  (if (= steps 1)
    [nil (+ arg reg)]
    [[instr (dec steps) arg] reg]))

(defn- execute [[instr steps & args :as cmd] reg]
  (cond 
    (= instr :noop) [nil reg]
    (= instr :addx) (execute-addx cmd reg)))

(defn run [commands]
  (loop [[c & cs :as call] commands
         [cmd reg] [nil 1]
         cycl 0
         akk {}]
    (cond 
      (and (nil? cmd) (nil? c)) (assoc akk cycl reg)
      (nil? cmd) (recur cs [c reg] cycl (assoc akk cycl reg))
      :otherwise (recur call (execute cmd reg) (inc cycl) (assoc akk cycl reg)))))

(defn part1 [commands]
  (let [res (run commands)]
    (apply + (map #(* % (res (dec %))) [20 60 100 140 180 220]))))

(defn part2 [commands]
  (let [res (run commands)]
    (for [c (range 1 (count res))]
      (let [cm (mod c 40) sprite (mod (res (dec c)) 40)]
        (if (or (= cm sprite) (= cm (inc sprite)) (= cm (+ 2 sprite)))
          "#"
          ".")))))

(defn- parse-command [line]
  (cond 
    (str/starts-with? line "addx") [:addx 2 (Integer. (str (subs line 5)))]
    (str/starts-with? line "noop") [:noop 1]))

(def commands (map parse-command (str/split-lines (slurp "resources/day10_input.txt"))))

(println "Day 10 - 1: " (part1 input)) ; 17940
(println "Day 10 - 2: " (map #(println %) (map #(apply str %) (partition 40 (part2 commands))))) ; ZCBAJFJZ
