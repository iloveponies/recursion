(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if(> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if(pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if(empty? a-seq)
    a-seq
    (if(pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if(empty? a-seq)
    a-seq
    (if(pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-2) (rest seq-1)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if(<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if(zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if(empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if(empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if(empty? a-seq)
    freqs
    (let [i (first a-seq)
          f (if(contains? freqs i)
              (assoc freqs i (inc (get freqs i)))
              (assoc freqs i 1))]
      (my-frequencies-helper f (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if(empty? a-map)
    '()
    (concat (repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if(or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
 (if(or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    (vector (my-take h a-seq) (my-drop h a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if(or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [h (halve a-seq)
          [a b] (map merge-sort h)]
      (seq-merge a b))))

(defn monotonic? [a-seq]
  (if(or (empty? a-seq) (singleton? a-seq))
    true
    (or (apply <= a-seq) (apply >= a-seq))))
  
(defn split-help [mon a-seq]
  (if(empty? a-seq)
    mon
    (let [all (take-while monotonic? (inits a-seq))
          l (last all)
          r (drop (count l) a-seq)]
      (split-help (cons l mon) r))))

(defn split-into-monotonics [a-seq]
  (reverse (split-help '() a-seq)))

(defn insert [a-seq item pos]
  (concat (take pos a-seq) [item] (drop pos a-seq)))
  
(defn insert-all [a-seq item]
  (map (fn [x] (insert a-seq item x)) (range (inc (count a-seq)))))
  
(defn permutations [a-set]
  (if(empty? a-set)
    '(())
    (apply concat (map (fn [p] (insert-all p (first a-set))) (permutations (rest a-set))))))

(defn insert-and-not [a-set item]
  (vector a-set (conj a-set item)))

(defn powerset [a-set]
  (if(empty? a-set)
    #{#{}}
    (apply concat (map (fn [p] (insert-and-not p (first a-set))) (powerset (rest a-set))))))

