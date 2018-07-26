(ns paip-clj.chap-1.ex-1
  (:require [clojure.string :as str]))

(defn last-name [full-name]
  "Exercise 1.1: an improved version of `last-name`. 
  Gives the `last-name` of the given `full-name`.
  Ex: (last-name '(John Smith Jr. MD)) = Smith"
  (loop [full-name full-name]
    (let [s-name (str (last full-name))]
      (if (str/ends-with? s-name ".")
        (recur (butlast full-name))
        (if (= s-name (str/upper-case s-name))
          (recur (butlast full-name))
          (last full-name))))))

(defn power [number expo]
  "Exercise 1.2: naive implementation of raising 
   `number` to the `expo` power."
  (if (= expo 0)
    1
    (loop [total 1
           expo expo]
      (if (= 1 expo)
        (* total number)
        (recur (* total number) (- expo 1))))))

(defn atom-count [atoms]
  "Exercise 1.3: Counts the total number of `atoms` in
  a passed expression."
  (loop [total 0
         atoms atoms]
    (if (empty? atoms)
      total
      (recur (+ 1 total) (rest atoms)))))

(defn count-anywhere [exp1 exp2]
  "Exercise 1.4: Count the number of times an expression `exp1`
  occurs within another expression `exp2`."
  (count (filter #(= % exp1) (flatten exp2))))

(defn dot-product [nums1 nums2]
  "Exercise 1.5: Compute the dot-product of `nums1` and `nums2`."
  (reduce + (map * nums1 nums2)))

