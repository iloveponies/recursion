(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond 
   (empty? coll) false
   (empty? (rest coll)) true
   :else false))

(defn my-last [coll]
  (cond
   (singleton? coll) (first coll)
   (empty? coll) nil
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (let [curr (first a-seq)]
    (cond 
     (singleton? a-seq) curr
     (empty? a-seq) nil
     :else (max curr (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [seq-1-size (count seq-1)
        seq-2-size (count seq-2)]
    (if (<= seq-1-size seq-2-size) seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (let [curr (first a-seq)]
    (cond 
     (singleton? a-seq) curr
     (empty? a-seq) nil
     :else (seq-max curr (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (let [curr (first a-seq)]
    (cond 
     (empty? a-seq) a-seq
     (pred? curr) (cons curr (my-filter pred? (rest a-seq)))
     :else (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [curr (first a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? curr) 
     (cons curr (my-take-while pred? (rest a-seq)))
     :else '())))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) 
   (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2)) 
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (== 0 k) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (== 0 n) 0
   (== 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) '()
   :else (cons what-to-repeat 
               (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if
   (<= up-to 0) '()
   (let [seuraava (- up-to 1)]
     (cons seuraava (my-range seuraava)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [a-seq-rev (reverse a-seq)]
    (map reverse (tails a-seq-rev))))

(defn rotations [a-seq]
 (let [rotate (fn [myseq] 
                (if (empty? myseq) '()
                  (concat (rest myseq) (cons (first myseq) '()))))
       rotations_ (fn ! [myseq2]
                    (let [new-seq (cons myseq2 '())]
                      (if (seq= myseq2 (seq a-seq))
                        new-seq
                        (concat new-seq 
                                (! (rotate myseq2))))))]
   (rotations_ (rotate a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  [:-])

(defn my-frequencies [a-seq]
  [:-])

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (cond
   (empty? coll) '()
   (<= n 0) '()
   :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (<= n 0) coll
   :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [lkm (int (/ (count a-seq) 2))]
    (cons (my-take lkm a-seq) (cons (my-drop lkm a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  (sort (concat a-seq b-seq)))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])