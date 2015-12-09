(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
     (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll)) (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
  (apply max (set a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil 
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
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
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
     (not (= (count a-seq) (count b-seq))) false
     (and (empty? a-seq) (empty? b-seq)) true
     (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
     :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) seq-1
    (empty? seq-2) seq-2
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= k 0) 1
    :else (* n (power n ( - k 1)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (find freqs (first a-seq))
                      (update-in freqs [(first a-seq)] inc)
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    {}
    (concat (repeat (second (first a-map))  (first (first a-map))) 
      (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond 
    (== n 0) '()
    (empty? coll) coll
    :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond (<= n 0) coll
    (empty? coll) coll
    :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [halfleng (int (/ (count a-seq) 2))]
	[(my-take halfleng a-seq) (my-drop halfleng a-seq)]))

(defn seq-merge [a-seq b-seq]
  (sort (concat a-seq b-seq)))

(defn merge-sort [a-seq]
  (cond (== (count a-seq) 0) '()
    (== (count a-seq) 1) a-seq
    :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) a-seq
    (let [prefixes  (rest (reverse (inits a-seq)))
      monotonic? (fn [seq] (or (apply <= seq) (apply >= seq)))
      longest_mono (last (take-while  monotonic? prefixes))] 
    (cons longest_mono (split-into-monotonics (my-drop (count longest_mono) a-seq))))))

(defn permutations [a-set]
 [:-])

(defn powerset [a-set]
  [:-])

