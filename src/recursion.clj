(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (= 1 (count coll)))

(defn my-last [coll]
  (if (empty? coll) nil 
    (if (singleton? coll) 
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) 
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [lcount (count seq-1)
        rcount (count seq-2)]
    (if (> lcount rcount) seq-1 seq-2)))

;; easier using reduce
(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) 
               (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq)) 
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn only-numbers? [coll]
  (cond
    (empty? coll) true
    (number? (first coll)) 
      (only-numbers? (rest coll))
    :else false))

(defn sequence-contains? [elem a-seq]
  (cond 
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) true
    (or 
      (and (empty? a-seq) (not (empty? b-seq)))
      (and (empty? b-seq) (not (empty? a-seq)))
      (not (= (first a-seq) (first b-seq)))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
    (cons (f (first seq-1) (first seq-2)) 
          (my-map f (rest seq-1) (rest seq-2)))))

;; Test function
(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

(defn power [n k]
  (cond 
    (zero? k) 1
    (< k 2) n
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (or (= 1 n) (= 2 n)) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1) '()
    (= 1 how-many-times) (list what-to-repeat) 
    :else (cons what-to-repeat 
                (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (cons (reverse (into () a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (let [seq-key (first a-seq)
          new-freq (if (contains? freqs seq-key)
            (assoc freqs seq-key (inc (get freqs seq-key)))
            (assoc freqs seq-key 1))]
      (my-frequencies-helper new-freq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat (fn [[x y]] (repeat y x)) (seq a-map)))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll)) '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (zero? n)) coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq) b (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) '()
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a b) (cons a (seq-merge (rest a-seq) b-seq))
      (> a b) (cons b (seq-merge a-seq (rest b-seq)))
      :else (concat (list a b) (seq-merge (rest a-seq) (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2) a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half) 
                 (merge-sort second-half)))))

(defn rotations-helper [a-seq n]
  (if (zero? n) '()
    (cons (concat (drop n a-seq) (take n a-seq)) 
          (rotations-helper a-seq (dec n)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
    (rotations-helper a-seq (count a-seq))))

;; Algorithm
;; 1. Reverse and find inits
;; 2. take-while with is-monotonic? as filter
;; 3. take the last one (assume of size k)
;; 4. drop k from original
;; 5. recurse
(defn is-monotonic? [a-seq]
  (if (< (count a-seq) 2) true
    (or (apply > a-seq) (apply < a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) '()
   (let [largest (last (take-while is-monotonic? (reverse (inits a-seq))))]
    (cons largest (split-into-monotonics (drop (count largest) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
