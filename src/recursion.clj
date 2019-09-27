(ns recursion)

(defn product [coll]
  (cond
    (empty? coll) 1
    :else (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond 
    (empty? coll) false
    :else (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond
    (> (count seq-1) (count seq-2)) seq-1
    :else seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond 
    (empty? a-seq) ()
    :else 
      (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (empty? a-seq) false
    (empty? b-seq) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) ()
    (empty? seq-2) ()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? k) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (<= n 1) n
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) ()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (<= up-to 0) ()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (defn rotations-1 [a-seq count]
    (cond
      (zero? count) ()
      ;:else (cons (seq a-seq) (rotations-1 (concat (rest a-seq) [(first a-seq)]) (dec count)))))
      :else (cons (concat (drop count a-seq) (take count a-seq)) (rotations-1 a-seq (dec count)))))
  
  (cond
    (empty? a-seq) '(())
    :else (rotations-1 (seq a-seq) (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else 
    (let [elem (first a-seq)
          elem-val (get freqs elem)]
      (if elem-val
        (my-frequencies-helper (assoc freqs elem (inc elem-val)) (rest a-seq))
        (my-frequencies-helper (assoc freqs elem 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [helper (fn [[k v]] (repeat v k))]
    (cond
      (empty? a-map) ()
      :else (concat (helper (first a-map)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (<= n 0) ()
    (empty? coll) ()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (<= n 0) coll
    (empty? coll) ()
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (vector (my-take mid a-seq) (my-drop mid a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [first-a (first a-seq)
        first-b (first b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< first-a first-b) (cons first-a (seq-merge (rest a-seq) b-seq))
      :else (cons first-b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) ()
    (singleton? a-seq) a-seq
    :else 
      (let [[h1 h2] (halve a-seq)]
        (seq-merge (merge-sort h1) (merge-sort h2)))))

; From structured_data.clj
(defn monotonic? [a-seq]
  (cond
    (empty? a-seq) true
    :else 
      (or (apply <= a-seq)
          (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (cond
    (empty? a-seq) ()
    :else 
      (let [elem (my-last (my-take-while monotonic? (inits a-seq)))
            to-drop (count elem)]
        (cons elem (split-into-monotonics (my-drop to-drop a-seq))))))

(defn permutations [a-set]
  (defn permutations-helper [elem a-seq]
    "prepend the elem to each member sequence of a-seq"
    (let [len (count a-seq)]
      (cond
        (zero? len) elem
        :else (map cons (repeat len elem) a-seq))))

  (cond
      (empty? a-set) '(())
      (singleton? a-set) (cons a-set ())
      :else (mapcat (fn[x] (permutations-helper (first x) (permutations (rest x)))) (rotations a-set))))

(defn powerset [a-set]
  (defn powerset-helper [elem a-set]
    (let [len (count a-set)
          e (set [elem])]
      (cond 
        (zero? len) e
        :else (map clojure.set/union (repeat len e) a-set))))
  
  (cond
    (empty? a-set) '#{#{}}
    :else 
      (let [e (first a-set)
            pt (powerset (rest a-set))]
        (clojure.set/union pt (powerset-helper e pt)))))













