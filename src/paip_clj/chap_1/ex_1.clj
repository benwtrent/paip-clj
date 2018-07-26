(ns paip-clj.chap-1.ex-1
  (:require [clojure.string :as str]))

(defn last-name [full-name]
  (loop [full-name full-name]
    (let [s-name (str (last full-name))]
      (if (str/ends-with? s-name ".")
        (recur (butlast full-name))
        (if (= s-name (str/upper-case s-name))
          (recur (butlast full-name))
          (last full-name))))))

(defn power [number expo]
  (if (= expo 0)
    1
    (loop [total 1
           expo expo]
      (if (= 1 expo)
        (* total number)
        (recur (* total number) (- expo 1))))))

(defn atom-count [atoms]
  (loop [total 0
         atoms atoms]
    (if (empty? atoms)
      total
      (recur (+ 1 total) (rest atoms)))))

