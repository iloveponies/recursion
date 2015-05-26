(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false (if (empty? (rest coll)) true false)))

(defn my-last [coll]
  (if (singleton? coll) (first coll) (if (empty? coll) nil 
                                         (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (singleton? a-seq) (first a-seq) (if (empty? a-seq) nil 
                                         (max (first a-seq) 
                                              (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (if (singleton? a-seq) (first a-seq) (if (empty? a-seq) nil
                                           (seq-max (first a-seq) 
                                                    (longest-sequence 
                                                       (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq (if (pred? (first a-seq)) 
			       (conj (my-filter pred? (rest a-seq)) 
                                     (first a-seq)) 
			       (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false 
    (not= (first a-seq) elem ) (sequence-contains? elem (rest a-seq))
    :else true))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) ()
    (pred? (first a-seq)) (conj (my-take-while pred? (rest a-seq)) 
						     (first a-seq))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (drop 1 a-seq)) 
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (not (empty? a-seq)) (empty? b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) () 
      (conj (my-map f (rest seq-1) (rest seq-2)) 
            (f (first seq-1) (first seq-2))) ))

(defn power [n k]
  (if (zero? k) 1 (* n (power n (dec k)))))

(defn fib [n]
  (cond (== n 0) 0
     (== n 1) 1
     :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) ()
      (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (if (<= up-to 0) () (conj (my-range (dec up-to)) (- up-to 1))))

(defn tails [a-seq]
  (if (empty? a-seq) (conj () a-seq) (conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [a-seq rots]
  (if (<= rots 0) (conj () a-seq) (conj (rotations-helper (conj (drop-last a-seq) 
                                                             (last a-seq)) 
                                                         (dec rots)) a-seq)))

(defn rotations [a-seq]
  (rotations-helper a-seq (dec (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs (if (find freqs (first a-seq)) 
                               (my-frequencies-helper 
                                 (update-in freqs [(first a-seq)] inc) 
                                 (rest a-seq)) 
                               (my-frequencies-helper (assoc freqs 
                                 (first a-seq) 1) 
                               (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [output a-map]
  (if (empty? a-map) output (un-frequencies-helper (concat output 
                                                     (repeat 
                                                       (val (first a-map))
                                                       (key (first a-map))))
                               (rest a-map))))

(defn un-frequencies [a-map]
  (un-frequencies-helper () a-map))

(defn my-take [n coll]
  (if (< (count coll) n) coll (if (== n 0) () (cons (first coll) 
                                                (my-take (dec n) (rest coll))))
    ))

(defn my-drop [n coll]
  (if (== n 0) coll (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (conj (vector (my-take (int (/ (count a-seq) 2)) a-seq)) 
        (my-drop (int (/ (count a-seq) 2)) a-seq)))

(defn seq-merge-helper [output a-seq b-seq]
  (cond (empty? a-seq) (concat output b-seq)
    (empty? b-seq) 
      (concat output a-seq)
    (< (first a-seq) (first b-seq)) 
      (seq-merge-helper (conj output (first a-seq)) (rest a-seq) b-seq)
    (> (first a-seq) (first b-seq))
       (seq-merge-helper (conj output (first b-seq)) a-seq (rest b-seq))
    (== (first a-seq) (first b-seq))
       (seq-merge-helper (conj output (first a-seq) (first b-seq)) 
                           (rest a-seq) (rest b-seq)) ))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1) a-seq (let [[f s] (halve a-seq)]
    (seq-merge (merge-sort f) (merge-sort s))) ))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq)) true false))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) a-seq (let [output (take-while monotonic? (inits a-seq))]
                          (cons output (split-into-monotonics 
                                         (drop (count output) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

