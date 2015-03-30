(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))  (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (nil? (my-last (rest coll))) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (nil? (max-element (rest a-seq))) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (nil? (longest-sequence (rest a-seq))) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) '()
   :else (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq))) (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== (first a-seq) elem) true
   :else (or false (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) ()
   :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k) 1
   (zero? (mod k 2)) (let [pp (power n (/ k 2))] (* pp pp))
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n ) 0
   (== n 1) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) ()
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [n (dec up-to)]
  (cond
   (<= up-to 0) ()
   :else (seq (conj (my-range n) n)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons (seq a-seq) (tails (rest (seq a-seq))))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))


(defn rec-rotations [left a-seq]
 (if (== left 1) (seq [a-seq]) (cons a-seq (rec-rotations (- left 1) (concat (rest a-seq) [(first a-seq)])))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(()) (rec-rotations (count a-seq) (seq a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (let [f (first a-seq)]
    (if (empty? a-seq) freqs (my-frequencies-helper (assoc freqs f (if (nil? (get freqs f)) 1 (inc (get freqs f)))) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [f (first a-map)]
    (if (empty? a-map) '() (concat (repeat (get f 1) (get f 0)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (== n 0)) '() (concat [(first coll)] (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (== n 0)) coll (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [s (int (/ (count a-seq) 2))]
    (seq [(my-take s a-seq) (my-drop s a-seq)])))

(defn seq-merge [a-seq b-seq]
  (let [fa (first a-seq)
        fb (first b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq)) '()
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (< fa fb) (concat [fa] (seq-merge (rest a-seq) b-seq ))
     :else (concat [fb] (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) '()
   (== (count a-seq) 1) a-seq
   :else (let [[fh lh] (halve a-seq)]
           (seq-merge (merge-sort fh) (merge-sort lh)))))


(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn rec-powerset [pas fut]
  (if
   (empty? fut) #{pas}
   (clojure.set/union (rec-powerset pas (rest fut)) (rec-powerset (conj pas (first fut)) (rest fut)))))

(defn powerset [a-set]
  (rec-powerset #{} a-set))
