(ns recursion)

(defn product [coll]
  (if (empty? coll)
	1
	(* (first coll) (product coll)))))

(defn singleton? [coll]
  (and(not (empty? coll)) (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
  (if (singleton? coll)
    (first coll)
   (my-last (rest coll)))))


(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq)(first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))


(defn seq-max [seq-1 seq-2]
  (let [size1 (count seq-1)
          size2 (count seq-2)]
      (if (> size1 size2)
        seq-1
        seq-2)))


(defn longest-sequence [a-seq]
  (if (< (count a-seq) 2)
    (my-last a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-map [f a-seq]
  (if (empty? a-seq)
    a-seq
    (cons (f (first a-seq))
          (my-map f (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first (first a-seq)
          rest  (my-filter pred? (rest a-seq))]
      (if (pred? first) (cons first rest) rest))))


(defn sequence-contains? [elem a-seq]
 (cond
     (empty? a-seq) false
     (= (first a-seq) elem) true
     :else
      (recur elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
   (if (empty? a-seq)
    a-seq
    (let [[x & xs] a-seq]
      (if (pred? x)
        (cons x (my-take-while pred? xs))
        []))))

(defn my-drop-while [pred? a-seq]
 (cond
    (empty? a-seq) '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
   (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or  (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else
    (seq= (rest a-seq) (rest b-seq))))


(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) '()
        :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
   (if (== 0 k)
    1
    (* n (power n (dec k)))))


(defn fib [n]
 (cond
    (<= n 0) 0
    (= n 1) 1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat 
          (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (cond (< 1 up-to) '()
        :else (cons (dec up-to) (my-range (dec up-to)))))


(cond (empty? a-seq) '(())
        :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (cond (empty? a-seq) '(())
        :else (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          cnt (inc (get freqs fst 0))]
      (my-frequencies-helper (assoc freqs fst cnt) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [m-key (key (first a-map))
          m-val (val (first a-map))]
      (concat (repeat m-val m-key) (un-frequencies(rest a-map))))))

(defn my-take [n coll]
  (if (and (> n 0) (not (empty? coll)))
    (cons (first coll) (my-take (dec n) (rest coll)))
    '()))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (recur (dec n) (rest coll))))

(defn halve [a-seq]
  (let [size-to-take (int (/ (count a-seq) 2))]
    [(my-take size-to-take a-seq)
     (my-drop size-to-take a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [current-a (first a-seq)
        current-b (first b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (<= current-a current-b) (cons current-a (seq-merge (rest a-seq) b-seq))
      (> current-a current-b)  (cons current-b (seq-merge a-seq (rest b-seq))))))



(defn merge-sort [a-seq]
 (if (<= (count a-seq) 1)
    a-seq
    (let [[seq1 seq2] (halve a-seq)]
      (seq-merge (merge-sort seq1) (merge-sort seq2)))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
 (if (>= 1 (count a-set))
    (list (into '() a-set))
    (for [head a-set
          tail (permutations (disj (set a-set) head))]
      (do (cons head tail)))))

(defn powerset [a-set]
   (if (empty? a-set)
    #{#{}}
    (let [first-set (conj #{} (first a-set))
          tail (powerset (rest a-set))]
      (clojure.set/union (map #(clojure.set/union first-set %) tail) tail))))

