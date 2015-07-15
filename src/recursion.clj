(ns recursion)

(defn product [coll]
  (if (empty? coll)
      1
      (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
  (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
     (first coll)
     (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
     (first a-seq)
     (let [greater (max (first a-seq) (second a-seq))]
       (max-element (cons greater (rest (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
     seq-1
     seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
     (first a-seq)
     (let [longer (seq-max (first a-seq) (second a-seq))]
       (longest-sequence (cons longer (rest (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (cond
     (empty? a-seq) a-seq
     (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
     :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
     (empty? a-seq) false
     (= (first a-seq) elem) true
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
     :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
     (and (empty? a-seq) (empty? b-seq)) true
     (or (empty? a-seq) (empty? b-seq)) false
     (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
     :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
      ()
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
      1
      (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
      ()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
      ()
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
      (cons () ())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
      (cons () ())
      (let [initials (reverse (rest (reverse a-seq)))]
         (cons (seq a-seq) (inits initials)))))

(defn rotations-with-iteration [a-seq n]
   (if (zero? n)
       ()
       (let [rotation (concat (rest a-seq) (vector (first a-seq)))]
          (cons (seq a-seq) (rotations-with-iteration rotation (dec n))))))
(rotations-with-iteration [1 2 3 4] 4)

(defn rotations [a-seq]
  (if (empty? a-seq)
      (cons () ())
      (rotations-with-iteration a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
      freqs
      (let [seuraava (first a-seq)
            new-freqs (if (contains? freqs seuraava)
                          (assoc freqs seuraava (inc (get freqs seuraava)))
                          (assoc freqs seuraava 1))]
        (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
      ()
      (let [seuraava (first a-map)
            seuraava-key (get seuraava 0)
            seuraava-freq (get seuraava 1)]
        (concat (repeat seuraava-freq seuraava-key) (un-frequencies (dissoc a-map seuraava-key))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
      ()
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
     (zero? n) (seq coll)
     (empty? coll) ()
     :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [puolivali (int (/ (count a-seq) 2))
        first-half (my-take puolivali a-seq)
        second-half (my-drop puolivali a-seq)]
    (conj [] first-half second-half)))

(defn seq-merge [a-seq b-seq]
  (cond
     (empty? a-seq) (seq b-seq)
     (empty? b-seq) (seq a-seq)
     (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
     (= (count a-seq) 0) ()
     (= (count a-seq) 1) (seq a-seq)
     :else (let [halves (halve a-seq)
            first-half (get halves 0)
            second-half (get halves 1)]
        (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn monotonic? [a-seq]
  (or (apply < a-seq) (apply > a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
     ()
     (let [prefixes (inits a-seq)
           monotonic-prefix (first (drop-while (fn [x] (not (monotonic? x))) prefixes))
           tail (drop (count monotonic-prefix) a-seq)]
       (concat (cons monotonic-prefix ()) (split-into-monotonics tail)))))

(defn permutations [a-seq]
  (cond
     (zero? (count a-seq)) (cons () ())
     (= 1 (count a-seq)) (vector a-seq)
     :else (for [alkio a-seq
                 loput (permutations (disj (set a-seq) alkio))]
            (cons alkio loput))))

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
      (let [remainder (powerset (next a-set))]
        (clojure.set/union remainder (map (fn [x] (conj x (first a-set))) remainder)))))


