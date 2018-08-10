(ns paip-clj.chap-2.grammars)

(defn one-of [lst]
  (list (rand-nth lst)))

(defn Prep []
  (one-of ["to" "in" "by" "with" "on"]))

(defn Adj []
  (one-of ["big" "little" "blue" "green" "adiabatic"]))

(defn Verb []
  (one-of ["hit" "took" "saw" "liked"]))

(defn Noun []
  (one-of ["man" "ball" "woman" "table"]))

(defn Article []
  (one-of ["the" "a"]))

(declare noun-phrase)

(defn PP []
  (concat (Prep) (noun-phrase))) (defn PP* [] (if (rand-nth '(true nil))
    (concat (PP) (PP*))
    nil))

(defn Adj* []
  (if (= (rand-int 2) 0)
    nil
    (concat (Adj) (Adj*))))

(defn noun-phrase []
  (concat (Article) (Adj*) (Noun) (PP*)))

(defn verb-phrase []
  (concat (Verb) (noun-phrase)))

(defn sentence []
  (concat (noun-phrase) (verb-phrase)))


(def simple-grammar
  {
    :sentence [[:noun-phrase :verb-phrase]] 
    :noun-phrase [[:article :noun ]]
    :verb-phrase [[:verb :noun-phrase]]
    :article ["the" "a"]
    :noun ["man" "ball" "woman" "table"]
    :verb ["hit" "took" "saw" "liked"]
  }
)

(def bigger-grammar
  {
    :sentence [[:noun-phrase :verb-phrase]] 
    :noun-phrase [[:article :adj* :noun :prep-phrase*]]
    :verb-phrase [[:verb :noun-phrase :prep-phrase*]]
    :prep-phrase* [[] [:prep-phrase :prep-phrase*]]
    :prep-phrase [[:prep :noun-phrase]]
    :prep ["to" "in" "by" "with" "on"]
    :adj* [[] [:adj :adj*]]
    :adj ["big" "little" "blue" "green" "adiabatic"]
    :article ["the" "a"]
    :noun ["man" "ball" "woman" "table"]
    :verb ["hit" "took" "saw" "liked"]
  }
)

(def grammar simple-grammar)

(defn generate [phrase]
  (cond 
    (vector? phrase) (mapcat generate phrase)
    (keyword? phrase) (generate (rand-nth (phrase grammar)))
    :else (vector (if (fn? phrase) (phrase) phrase))))

(defn generate-tree [phrase]
  (cond
        (vector? phrase) (mapcat generate-tree phrase)
        (keyword? phrase) (cons phrase (generate-tree (rand-nth (phrase grammar))))
        :else (vector phrase)))


(defn generate-all [phrase] ;currently getting a stack overflow?
  (cond 
        (nil? phrase) [nil]
        (vector? phrase) (for [x (generate-all (first phrase)) 
                               y (generate-all (into [] (rest phrase)))] (vector x y))
        (keyword? phrase) (mapcat generate-all (phrase grammar))
        :else (vector (vector phrase))))

