(ns recursion)

(defn product [coll]
   (if (empty? coll)
     1
     (* (first coll)
        (product (rest coll)))))

(defn singleton? [coll]
  (cond
   (empty? coll) false
   (empty? (rest coll)) true
   :else false))

(defn my-last [coll]
   (if (empty? (rest coll))
     (first coll)
     (my-last (rest coll))))

(defn max-element [a-seq]
   (if (empty? (rest a-seq))
     (first a-seq)
     (max (first a-seq)
          (max-element (rest a-seq)))))

(defn longer? [seq-1 seq-2]
   (cond
    (empty? seq-1) false
    (empty? seq-2) true
    :else (longer? (rest seq-1) (rest seq-2))))

(defn seq-max [seq-1 seq-2]
   (if (longer? seq-1 seq-2)
     seq-1
     seq-2))

(defn longest-sequence [a-seq]
   (let [first-elem (first a-seq)
         next-elem (first (rest a-seq))
         tail (rest (rest a-seq))]
    (if (empty? tail)
      (seq-max first-elem next-elem)
      (seq-max first-elem
               (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
   (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
   (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
   (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred?
                                        (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
   (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (= (first a-seq) (first b-seq))
         (and (not (empty? a-seq)) (not (empty? b-seq)))) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
   (if (and (not (empty? seq-1))
            (not (empty? seq-2)))
     (cons (f (first seq-1) (first seq-2))
           (my-map f (rest seq-1) (rest seq-2)))
     '()))

(defn power [n k]
  (cond
    (zero? n) 0
    (zero? k) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
   (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
   (if (< how-many-times 1)
     '()
     (cons what-to-repeat
           (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
   (if (< up-to 1)
     '()
     (cons (- up-to 1)
           (my-range (dec up-to)))))

(defn tails [a-seq]
   (if (empty? a-seq)
     '(())
     (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
   (if (empty? a-seq)
     '(())
     (cons a-seq (inits (butlast a-seq)))))

(defn rotations-helper [n a-seq]
   (if (== n 0)
     '()
     (let [rotated (concat (rest a-seq)
                           [(first a-seq)])]
       (concat [a-seq]
               (rotations-helper (dec n) rotated)))))

(defn rotations [a-seq]
   (if (empty? a-seq)
     '(())
     (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
   (if(empty? a-seq)
     freqs
    (let [elem (first a-seq)
         occ (freqs elem)]
     (if (boolean occ)
       (my-frequencies-helper (assoc freqs elem  (inc occ))
                              (rest a-seq))
       (my-frequencies-helper (assoc freqs elem 1)
                              (rest a-seq))))))

(defn my-frequencies [a-seq]
   (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
   (if (empty? a-map)
     '()
     (let [elem (first a-map)
           cey (key elem)
           wal (val elem)]
       (concat (repeat wal cey)
               (un-frequencies (rest a-map))))))

(defn my-take [n coll]
   (if (or (empty? coll) (== n 0))
     '()
     (cons (first coll)
           (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (== n 0))
     coll
     (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
   (let [seq-len (count a-seq)
         n (int (/ seq-len 2))]
     (concat [(my-take n a-seq)]
             [(my-drop n a-seq)])))

(defn seq-merge [a-seq b-seq]
   (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else  (let [a-elem (first a-seq)
                 b-elem (first b-seq)]
             (if (> a-elem b-elem)
               (cons b-elem (seq-merge a-seq
                                       (rest b-seq)))
               (cons a-elem (seq-merge (rest a-seq)
                                       b-seq))))))

(defn merge-sort [a-seq]
   (if (or (empty? a-seq) (== (count a-seq) 1))
     a-seq
     (let [[firzt lazt] (halve a-seq)]
       (seq-merge (merge-sort firzt)
                (merge-sort lazt)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])




