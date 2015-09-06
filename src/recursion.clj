(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [first-item (first a-seq)]
    (cond
      (empty? a-seq) nil
      (singleton? a-seq) first-item
      :else (seq-max first-item (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    ()
    (let [filtered-trail (my-filter pred? (rest a-seq))]
      (let [first-item (first a-seq)]
        (if (pred? first-item)
          (cons first-item filtered-trail)
     	  filtered-trail)))))
      
(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (== elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (let [take-while-trail (my-take-while pred? (rest a-seq))]
      (let [first-item (first a-seq)]
        (if (pred? first-item)
          (cons first-item take-while-trail)
     	  ())))))
	  
(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

; only for non-negative integer k
(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (let [dec-n (dec n)]
    (cond
      (= n 0) 0
      (= n 1) 1
      :else (+ (fib dec-n) (fib (dec dec-n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) ()
    (= how-many-times 1) (vector what-to-repeat)
    :else (cons what-to-repeat
                (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= up-to 0) ()
    (= up-to 1) (vector 0)
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (vector ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (vector ())
    (rest (my-map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [old-count (freqs (first a-seq))]
      (let [new-count (if (= old-count nil)
                        1
                        (inc old-count))]
      (let [new-freqs (assoc freqs (first a-seq) new-count)]
        (my-frequencies-helper new-freqs (rest a-seq)))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[what times] (first a-map)]
      (concat (repeat times what) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    ()
    (let [trail (my-take (dec n) (rest coll))]
      (cons (first coll) trail))))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (<= n 0) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-size (int (/ (count a-seq) 2))]
    (cons (my-take half-size a-seq) (vector (my-drop half-size a-seq)))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (let [first-a-item (first a-seq)]
        (let [first-b-item (first b-seq)]
          (if (<= first-a-item first-b-item)
            (cons first-a-item (seq-merge (rest a-seq) b-seq))
	    (cons first-b-item (seq-merge a-seq (rest b-seq)))))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [piece (first (filter monotonic? (inits a-seq)))]
      (cons piece (split-into-monotonics (drop (count piece) a-seq))))))

(defn permutations-helper [coll1 coll2]
  (if (empty? coll2)
    (conj '() coll1)
      (apply concat
        (map (fn [x] 
          (let [new-coll1 (conj coll1 x)]
            (let [new-coll2 (remove (fn [y] (= x y)) coll2)]
              (permutations-helper (conj coll1 x) new-coll2)))) coll2))))

(defn permutations [a-set]
  (permutations-helper '() a-set))

(defn powerset
  [a-set]
  (if (empty? a-set)
    '#{#{}}
    (let [intermediate (powerset (rest a-set))]
      (apply merge
        intermediate
        (map (fn [x] (conj x (first a-set))) intermediate)))))
