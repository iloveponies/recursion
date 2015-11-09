(ns recursion)

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

(defn consecutives [a-seq]
  (map vector a-seq (rest a-seq)))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
    (if (empty? a-seq)
      a-seq
      (if (pred? (first a-seq))
        (cons (first a-seq)
              (my-filter pred? (rest a-seq)))
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
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons (first a-seq)
          (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (and (and (not (nil? (first a-seq)))
             (not (nil? (first b-seq))))
        (= (first a-seq) (first b-seq)))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k)
     1
   (== 1 k)
     n
   :else
     (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (zero? (count a-seq))
    (vector [])
    (cons a-seq (tails (rest a-seq)))))

(defn inits-helper [a-seq]
  (let [follower (rest (reverse a-seq))]
    (if (zero? (count a-seq))
      (vector [])
      (cons a-seq (inits-helper (reverse follower))))))

(defn inits [a-seq]
  (reverse (inits-helper a-seq)))

 (defn rotations [a-seq]
   (let [beginnings (inits a-seq)
         endings (tails a-seq)
         unify (fn [x y]
                 (if (empty? x)
                   y
                   (if (not (empty? y))
                     (apply concat [x y])
                     x)))]
     (seq (set (map unify endings beginnings)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [a-key (first a-seq)]
    (if (empty? a-seq)
      freqs
      (let [freqs-copy (if (contains? freqs a-key)
                         (assoc freqs a-key (inc (get freqs a-key)))
                         (assoc freqs a-key 1))]
        (my-frequencies-helper freqs-copy (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat
     (repeat (second (first a-map)) (first (first a-map)))
     (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
   (or (empty? coll) (zero? n))
     '()
   (= 1 n)
     (cons (first coll) '())
   :else
     (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll)
     '()
   (> n 0)
     (my-drop (dec n) (rest coll))
   :else
     (cons (first coll) (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     '()
   (or (empty? b-seq)
       (and (not (empty? a-seq)) (< (first a-seq) (first b-seq))))
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge
       (merge-sort (first halves)) (merge-sort (second halves))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [beginnings (my-drop 1 (inits a-seq))
          monotony (last (my-take-while monotonic? beginnings))
          remaining (my-drop (count monotony) a-seq)]
      (cons monotony (split-into-monotonics remaining)))))

(defn permutations-helper [bgn nd]
  (if (< (count nd) 2)
    '()
    (let [ends (rotations nd)]
      (for [end ends]
        (let [rslt (if (seq? bgn)
                     (concat bgn end)
                     (conj end bgn))
              new-bgn-len (if (seq? bgn)
                            (inc (count bgn))
                            2)
              new-bgn (take new-bgn-len rslt)
              new-end (drop 1 end)]
          (if (seq? (first rslt))
            (concat rslt (permutations-helper new-bgn new-end))
            (concat (conj [] rslt) (permutations-helper new-bgn new-end))))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '([])
    (let [pmts (for [el (rotations a-set)]
                      (permutations-helper (first el) (rest el)))
          flat-pmts (flatten pmts)
          splitted (loop [seq1 []
                          seq2 flat-pmts]
                     (cond
                      (empty? seq2)
                        seq1
                      :else
                        (recur
                         (conj seq1 (take (count a-set) seq2))
                         (drop (count a-set) seq2))))]
      (seq (set splitted)))))

(defn powerset [a-set]
  (let [pmts (permutations a-set)
        bgns (apply concat (for [pmt pmts]
               (inits pmt)))
        sets (loop [target #{}
                    stuff  bgns]
               (cond
                (empty? stuff)
                  target
                :else
                  (recur (conj target (set (first stuff))) (rest stuff))))]
    sets))

