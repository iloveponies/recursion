(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (singleton? coll) (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
    (if (= (first a-seq) elem) true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (if (not (pred? (first a-seq))) 
      (empty a-seq)
      (cons (first a-seq) (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (if (not (pred? (first a-seq))) 
      a-seq
      (my-drop-while pred? (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) seq-1
    (empty? seq-2) seq-2
    :else (cons (f (first seq-1) (first seq-2)) 
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= k 0) 1
    (= n 0) 0
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else  (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1) []
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) 
    ()
    (cons (dec up-to) 
          (my-range (dec up-to)))))

(defn tails-helper [a-seq]
  (if (empty? a-seq) 
    a-seq
    (cons a-seq (tails-helper (rest a-seq)))))

(defn tails [a-seq]
  (cons [] (tails-helper a-seq)))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rot-helper [a-seq times]
  (let [rotate (fn [b-seq] (concat (rest b-seq) [(first b-seq)]))]
    (if (= times 1)
      (seq [a-seq])
      (cons (seq a-seq) (rot-helper (rotate a-seq) (dec times))))))

(defn rotations [a-seq]
  (if (< (count a-seq) 2)
    (list a-seq)
    (rot-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [eka (first a-seq)
          loput (rest a-seq)
          uusi (assoc freqs eka 1)]
      (if (contains? freqs eka)
        (my-frequencies-helper 
          (assoc freqs eka (inc (get freqs eka))) loput)
        (my-frequencies-helper uusi loput)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [helper (fn [[map-key map-val]] (my-repeat map-val map-key))]
    (if (empty? a-map)
      []
      (concat (helper (first a-map)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (empty? coll) coll
    (<= n 0) (empty coll)
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) coll
    (<= n 0) coll
    (== n 1) (rest coll)
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) a-seq
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) 
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [halves (halve a-seq)
          [left right] (map merge-sort halves)]
   	  (seq-merge left right))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn rotate-seq [a-seq]
  (concat (rest a-seq) [(first a-seq)]))

(defn perm-helper [start-seq a-seq times]
  (cond
    (empty? a-seq) start-seq
    (<= times 0) (empty a-seq)
    :else
;      (concat start-seq(perm-helper start-seq (rotate-seq a-seq) (dec times)))))
;          (conj start-seq (first a-seq)) (rest a-seq) (count (rest a-seq))))
      (concat (perm-helper start-seq (rotate-seq a-seq) (dec times))
          (perm-helper (concat start-seq (first a-seq)) (rest a-seq) (count (rest a-seq))))))

(defn permutations [a-set]
  (perm-helper () a-set (count a-set)))

(defn perm-helper [start-seq a-seq times]
  (cond
    (empty? a-seq) start-seq
    (<= times 0) (empty a-seq)
    :else
;      (concat start-seq(perm-helper start-seq (rotate-seq a-seq) (dec times)))))
;          (conj start-seq (first a-seq)) (rest a-seq) (count (rest a-seq))))
      (concat (perm-helper start-seq (rotate-seq a-seq) (dec times))
          (perm-helper (concat start-seq (first a-seq)) (rest a-seq) (count (rest a-seq))))))

(defn permutations [a-set]
  (perm-helper () a-set (count a-set)))

(defn perm-helper [start-seq a-seq times]
  (cond
    (empty? a-seq) start-seq
    (<= times 0) (empty a-seq)
    :else
;      (concat start-seq(perm-helper start-seq (rotate-seq a-seq) (dec times)))))
;          (conj start-seq (first a-seq)) (rest a-seq) (count (rest a-seq))))
      (concat (perm-helper start-seq (rotate-seq a-seq) (dec times))
          (perm-helper (concat start-seq (first a-seq)) (rest a-seq) (count (rest a-seq))))))

(defn permutations [a-set]
  (perm-helper () a-set (count a-set)))

(defn powerset [a-set]
  [:-])