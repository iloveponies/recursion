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
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem
      (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not (pred? (first a-seq))) []
   :else
     (cons (first a-seq)
           (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)
        (empty? b-seq)) true
   (empty? a-seq) false
   (empty? b-seq) false
   (not= (first a-seq)
         (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (empty? seq-1) []
   (empty? seq-2) []
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 2))
            (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< 0 how-many-times)
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))
    []))

(defn my-range [up-to]
  (cond
   (zero? up-to) []
   :else (cons (dec up-to)
               (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (cons a-seq
          (inits (reverse (rest (reverse a-seq)))))))

(defn inits-r [a-seq]
  (reverse (inits a-seq)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [a-seq]
    (map (fn [i] (concat (drop i a-seq) (take i a-seq) )) (range (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a-key (first a-seq)
          new-freqs (if (contains? freqs a-key)
                      (assoc freqs a-key (inc (get freqs a-key)))
                      (assoc freqs a-key 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    [] ;empty sequence
    (let [a-key (key (first a-map))
          a-val (val (first a-map))]
      (concat (my-repeat a-val a-key)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (empty? coll) coll
   (zero? n) (empty coll)
   :else (cons (first coll)
               (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) coll
   (zero? n) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [jako (int (/ (count a-seq) 2))]
    [(my-take jako a-seq) (my-drop jako a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a-val (first a-seq)
               b-val (first b-seq)]
           (if (< a-val b-val)
             (cons a-val (seq-merge (rest a-seq) b-seq))
             (cons b-val (seq-merge a-seq (rest b-seq)))
             ))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[half-seq1 half-seq2] (halve a-seq)]
      (seq-merge (merge-sort half-seq1) (merge-sort half-seq2)))))

(defn increasing? [a-seq]
  (cond
   (empty? (rest a-seq)) true
   (> (first a-seq) (second a-seq)) false
   :else (increasing? (rest a-seq))))

(defn decreasing? [a-seq]
  (increasing? (for [x a-seq] (- x))))

(defn monotonic? [a-seq]
  (or (increasing? a-seq)
      (decreasing? a-seq)))

(defn split-into-monotonics [a-seq]
  (cond
   (empty? a-seq) []
   (empty? (rest a-seq)) [a-seq]
   :else (let [monoseq (last (my-take-while monotonic? (inits-r a-seq)))]
      (cons monoseq (split-into-monotonics(my-drop (count monoseq) a-seq))))))

(defn permutations [a-set]
  (cond
   (empty? a-set) [a-set]
   (empty? (rest a-set)) [a-set]
   :else (let [n (count a-set)
               subseqs (map (fn [i] (concat (my-take i a-set) (my-drop (inc i) a-set))) (range n))
               subperms (map permutations subseqs)]
           (apply concat (map (fn [jth-elem seq-j] (map (fn [subperm-k] (cons jth-elem subperm-k)) seq-j)) a-set subperms)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{a-set}
    (let [pow-subset (powerset (rest a-set))]
      (clojure.set/union pow-subset
                         (map (fn [x] (conj x (first a-set))) pow-subset)))))
















