(ns aoc-2022.day11
  (:require [clojure.string :as str]))

(defn- apply-operation [worry-level [l ops r] divby bored? magic]
  (let [lv (if (= l "old") worry-level (Integer. l)) 
        rv (if (= r "old") worry-level (Integer. r))
        res (if (= ops "*") (* lv rv) (+ lv rv))]
    (if bored? res (mod res magic))))

(defn- bored [worry-level]
  (quot worry-level 3))

(defn- throw-from [monkey monkeys]
  (let [new-monkey (update (nth monkeys monkey) 0 (fn [a] (rest a)))]
    (update monkeys monkey (fn [_] new-monkey))))
  
(defn- throw-to [worry-level monkey monkeys]
  (let [new-monkey (update (nth monkeys monkey) 0 (fn [a] (cons worry-level a)))]
    (update monkeys monkey (fn [_] new-monkey))))

(defn- inspect-items [monkey monkeys bored? magic]
  (let [[worry-levels ops divby ift iff] (nth monkeys monkey)]
    (loop [[worry-level & wls] worry-levels
           new-monkeys monkeys]
      (if (nil? worry-level)
        new-monkeys
        (let [nwl (apply-operation worry-level ops divby bored? magic)
              new-worry-level (if bored? (bored nwl) nwl)]
          (if (zero? (mod new-worry-level divby))
            (recur wls (throw-to new-worry-level ift (throw-from monkey new-monkeys)))
            (recur wls (throw-to new-worry-level iff (throw-from monkey new-monkeys)))))))))

(defn- round [monkeys inspected bored? magic]
  (loop [n 0
         new-monkeys monkeys
         new-inspected inspected]
    (if (= n (count monkeys))
      [new-monkeys new-inspected]
      (recur (inc n) (inspect-items n new-monkeys bored? magic) (update new-inspected n (fn [a] (+ a (count (first (nth new-monkeys n))))))))))


(defn part1 [monkeys]
  (loop [new-monkeys monkeys
         inspected (into {} (for [i (range (count monkeys))] [i 0]))
         n 20]
    (if (zero? n)
      (apply * (take-last 2 (sort (vals inspected))))
      (let [[nm in] (round new-monkeys inspected true nil)]
        (recur nm in (dec n))))))

(defn part2 [monkeys]
  (let [magic (apply * (map #(nth % 2) monkeys))]
    (loop [new-monkeys monkeys
           inspected (into {} (for [i (range (count monkeys))] [i 0]))
           n 10000]
      (if (zero? n)
        (apply * (take-last 2 (sort (vals inspected))))
        (let [[nm in] (round new-monkeys inspected false magic)]
          (recur nm in (dec n)))))))

(defn- parse-starting-items [line]
  (map #(Integer. %) (rest (str/split line #", |: "))))

(defn- parse-operation [line]
  (take-last 3 (str/split line #" ")))

(defn- parse-last-number [line]
  (Integer. (last (str/split line #" "))))

(defn- parse-monkey [[line1 line2 line3 line4 line5 line6]]
  (let [starting-items (parse-starting-items line2)
        operation (parse-operation line3)
        divisible-by (parse-last-number line4)
        if-true (parse-last-number line5)
        if-false (parse-last-number line6)]
    [starting-items operation divisible-by if-true if-false]))

(def lines (str/split-lines (slurp "resources/day11_input.txt")))
(def monkeys (into [] (map parse-monkey (partition 6 (filter #(not (str/blank? %)) lines)))))

(println "Day 11 - 1: " (part1 input)) ; 90882
(println "Day 11 - 2: " (part2 input)) ; 30893109657
