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
  (concat (Prep) (noun-phrase)))

(defn PP* []
  (if (rand-nth '(true nil))
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
    :sentence [[noun-phrase verb-phrase]]
    :noun-phrase [[:article :noun]]
    :verb-phrase [[:verb noun-phrase]]
    :article ["the" "a"]
    :noun ["man" "ball" "woman" "table"]
    :verb ["hit" "took" "saw" "liked"]
  }
)
 
(def grammar simple-grammar)

(defn generate [phrase] ;this is not quiet right...something is off
  (cond 
    (vector? phrase) (mapcat generate phrase)
    (keyword? phrase) (generate (rand-nth (phrase grammar)))
    :else (vector (if (fn? phrase) (phrase) phrase))))


