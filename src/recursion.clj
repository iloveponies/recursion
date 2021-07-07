(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (or (empty? coll ) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
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
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1))
            (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (or (empty? a-seq) (nil? a-seq))
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [tails (tails (reverse a-seq))]
    (map reverse tails)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [vec-tails (vec (map vec (tails a-seq)))
          vec-inits (vec (map vec (reverse (inits a-seq))))]
      (rest (map concat vec-tails vec-inits)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [helper (fn [x] (my-frequencies-helper (assoc freqs (first a-seq) x) (rest a-seq)))]
    (cond
     (empty? a-seq) freqs
     (contains? freqs (first a-seq)) (helper (inc (get freqs (first a-seq))))
     :else (helper 1))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (into [] (concat [(first coll)] (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (cond
   (zero? n) coll
   (>= n (count coll)) ()
   (== (mod (count coll) 2) 0) (reverse (my-take n (reverse coll)))
   :else (reverse (my-take (inc n) (reverse coll)))))

(defn halve [a-seq]
  (if (empty? a-seq)
    []
    (let [middle-number (int (/ (count a-seq) 2))]
      [(my-take middle-number a-seq) (my-drop middle-number a-seq)])))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) ()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn monotonics-helper [a-seq]
  (cond
   (empty? a-seq) '(())
   (singleton? a-seq) (first a-seq)
   (or (apply <= (first a-seq)) (apply >= (first a-seq))) (first a-seq)
   :else (monotonics-helper (rest a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [longest-monotone (monotonics-helper (inits a-seq))]
      (cons longest-monotone
            (split-into-monotonics (drop (count longest-monotone) a-seq))))))

(defn permutations [a-set]
  (if (or (empty? a-set) (== 1 (count a-set)))
    (list a-set)
    (for [head a-set
          tail (permutations (disj (set a-set) head))]
      (cons head tail))))

(defn comb [k l]
  (if (= 1 k) (map vector l)
      (apply concat
             (map-indexed
              #(map (fn [x] (conj x %2))
                    (comb (dec k) (drop (inc %1) l)))
              l))))

(defn powerset [a-set]
  (conj (apply concat
         (for [x (range 1 (inc (count a-set)))]
           (map #(into #{} %) (comb x a-set)))) #{}))
