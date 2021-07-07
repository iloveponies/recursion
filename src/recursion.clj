(ns recursion)

(defn product [coll]
  (if (empty? coll) 
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (let [not-empty? (complement empty?)]
    (and
     (not-empty? coll)
     (empty? (rest coll)))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) 
                   (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (<= count-1 count-2)
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) 
                  (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond 
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond 
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons 
                          (first a-seq) 
                          (my-take-while pred? (rest a-seq)))
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
   (not= (first a-seq) (first b-seq)) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
          (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k) 1
      (* n (power n (dec k)))))

(defn fib [n]
  (if (<= 0 n 1) n
      (+ (fib (dec n)) 
         (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '()
    (cons what-to-repeat 
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0) '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
 (if (empty? a-seq) '(())
      (cons (seq a-seq) (inits (butlast a-seq)))))

(defn my-cycle [a-seq]
  (concat (rest a-seq) (list (first a-seq))))

(defn rotations-helper [count a-seq]
  (if (= count 0) '()
      (cons (seq a-seq) (rotations-helper (dec count) (my-cycle a-seq)))))

(defn rotations [a-seq]
  (if 
      (empty? a-seq) '(())
      (rotations-helper (count a-seq) a-seq)))

(defn nil->zero [x]
  (if (nil? x) 0 x))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
      (let [head (first a-seq)
            tail (rest a-seq)
            freq (nil->zero (get freqs head))]
        (my-frequencies-helper (assoc freqs head (inc freq)) tail))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat #(repeat (second %) (first %))  a-map))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll)) '()
      (cons (first coll) (my-take (dec n)  (rest coll)))))

(defn my-drop [n coll]
  (if (= 0 n) coll
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (/ (count a-seq) 2)]
    (split-at (int n) a-seq)))

(defn seq-merge [a-seq b-seq]
  (cond 
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a-head (first a-seq)
         b-head (first b-seq)
         a-tail (rest a-seq)
         b-tail (rest b-seq)]
     (if (<= a-head b-head) 
       (cons a-head (seq-merge a-tail b-seq))
       (cons b-head (seq-merge a-seq b-tail))))))

(defn merge-sort [a-seq]
  (let [[fst snd] (halve a-seq)]
    (cond
     (empty? fst) snd
     (empty? snd) fst
     :else (seq-merge (merge-sort fst)
                      (merge-sort snd)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

