(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
      (* (first coll) (product (rest coll)))))
 ;; exercise 2
 ;; TRACE t3295: (recursion/product [1 2 3])
 ;; TRACE t3296: | (recursion/product (2 3))
 ;; TRACE t3297: | | (recursion/product (3))
 ;; TRACE t3298: | | | (recursion/product ())
 ;; TRACE t3298: | | | => 1
 ;; TRACE t3297: | | => 3
 ;; TRACE t3296: | => 6
 ;; TRACE t3295: => 6

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
      (let [first-val (first a-seq)
            rest-seq (my-filter pred? (rest a-seq))]
        (if (pred? first-val) (cons first-val rest-seq) rest-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq)))) '()
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (if (or (empty? a-seq) (empty? b-seq)) 
    (and (empty? a-seq) (empty? b-seq))
    (and  (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    '()
    (cons (f (first seq-1) (first seq-2)) 
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 
    1
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
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) 
    (cons '() '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reversed-seq (reverse a-seq)
        rest-seq (reverse (rest reversed-seq))]
    (if (empty? a-seq) 
      (cons '() '())
      (concat (inits rest-seq) [a-seq]))))

(defn rotations [a-seq]
  (let [tailseqs (tails a-seq)
        initseqs (inits a-seq)]
    (set (my-map concat tailseqs initseqs))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) 
    freqs
    (let [first-val (first a-seq)
          count (if (contains? freqs first-val) 
                  (inc (freqs first-val))
                  1)
          new-freqs (assoc freqs first-val count)]
     (my-frequencies-helper new-freqs (rest a-seq)) )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
      (let [[val n] (first a-map)]
        (concat (repeat n val) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll)) 
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [first-half (int (/ (count a-seq) 2))]
    [(my-take first-half a-seq) (my-drop first-half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        :else (let [take-a (< (first a-seq) (first b-seq))
                    seq-used (if take-a a-seq b-seq)
                    seq-not-used (if take-a b-seq a-seq)
                    next-val (first seq-used)]
                (cons next-val (seq-merge (rest seq-used) seq-not-used)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[half1 half2] (halve a-seq)]
      (seq-merge (merge-sort half1) (merge-sort half2)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [monotonic? #(or (apply <= %) (apply >= %) )
          monotonics (take-while monotonic? (rest (inits a-seq)))
          last-mono (my-last monotonics)
          rest-seq (drop (count last-mono) a-seq)]
      (cons last-mono (split-into-monotonics rest-seq)))))

(defn cons-on-sets [to-cons sets]
  (map #(cons to-cons %) sets))

(defn permutations [a-set]
  (if (>= 1 (count a-set)) 
    [a-set]
    (let [rots (rotations a-set)
          rest-permutations #(cons-on-sets 
                              (first %) 
                              (permutations (rest %)))]
      (apply concat (map rest-permutations rots)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{a-set}
    (let [rest-rotations (map #(set (rest %)) (rotations a-set))
          rest-powersets (map powerset rest-rotations)] 
      (set (cons (set a-set) (apply concat rest-powersets))))))

