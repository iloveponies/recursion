(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (not (or (empty? coll) (not (empty? (rest coll))))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))


(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (singleton? (rest a-seq))
        (max (first a-seq) (my-last a-seq))
        (max (first a-seq) (max-element (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (singleton? (rest a-seq))
        (seq-max (first a-seq) (my-last a-seq))
        (seq-max (first a-seq) (longest-sequence (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
      true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     '()))

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
   (or (nil? a-seq) (nil? b-seq))
     false
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (or (nil? (first a-seq)) (nil? (first b-seq)))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
    false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (nil? seq-1) (nil? seq-2))
     '()
   (or (empty? seq-1) (empty? seq-2))
     '()
   (or (nil? (first seq-1)) (nil? (first seq-2)))
     '()
   :else
     (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== 0 n)
     0
   (== 1 n)
     1
   :else
     (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (> 1 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails-c [a-seq]
  (if (empty? a-seq)
    '()
    (cons (seq a-seq) (tails-c (rest a-seq)))))

(defn tails [a-seq]
  (conj (vec (map vec (tails-c a-seq))) [])) ;stupid expression to get [] at the end of sequence

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotate [a-sek]
  (concat (rest a-sek) (cons (first a-sek) '())))

(defn spinner [a-sek rounds]
  (if (== rounds 0)
    '()
    (cons a-sek (spinner (rotate a-sek) (dec rounds)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (conj '() []) ;stupid expression to get f-king (())
    (spinner (seq a-seq) (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [occurrences (fn [fr sq]
                        (if (contains? fr (first sq))
                          (assoc fr (first sq) (inc (get fr (first sq))))
                          (assoc fr (first sq) 1)))]
      (my-frequencies-helper (occurrences freqs a-seq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop [n coll]
  (cond
   (empty? coll)
     '()
   (== n 0)
     coll
   :else
     (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    [(my-take mid a-seq) (my-drop mid a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     '()
   (empty? a-seq)
     b-seq
   (empty? b-seq)
     a-seq
   (< (first a-seq) (first b-seq))
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (== (count a-seq) 1))
    a-seq
    (let [[alku loppu] (halve a-seq)]
      (seq-merge (merge-sort alku) (merge-sort loppu)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

