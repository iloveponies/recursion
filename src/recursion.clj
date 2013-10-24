(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
      (product (rest coll)))))

(defn depth [coll]
 (if (empty? coll)
   0
   (+ 1 (depth (rest coll)))))

(defn singleton? [coll]
  (= (depth coll) 1))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (depth seq-1) (depth seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq))) (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not (pred? (first a-seq))) ()
   :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn return-rest [a-seq]
  (if (empty? a-seq)
    a-seq
    (cons (first a-seq) (return-rest (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
   (if (not (pred? (first a-seq))) (return-rest a-seq) (my-drop-while pred? (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (and (empty? seq-1) (empty? seq-2)) seq-1
   (and (empty? seq-1) (not (empty? seq-2))) seq-1
   (and (not (empty? seq-1)) (empty? seq-2)) seq-2
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (== up-to 0)
    ()
   (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (vector [])
    (cons (apply vector a-seq) (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
    (vector ())
    (cons (seq a-seq) (inits (butlast a-seq)))))

(defn rotations-helper [a-seq n]
  (if (<= n 0)
    []
    (let [current (into [] (concat (take-last 1 a-seq) (take (dec (count a-seq)) a-seq)))]
      (cons current (rotations-helper current (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
  (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (assoc (my-frequencies-helper freqs (rest a-seq))
     (first (first a-seq))
     (count (filter (fn [elem] (= elem (first (first a-seq)))) (first a-seq))))))

(defn my-frequencies [a-seq]
 (if (empty? a-seq)
   {}
   (my-frequencies-helper {} (rotations a-seq))))


(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[a b] (first a-map)]
      (concat (repeat b a) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) coll
   (<= n 0)  coll
   :else (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
   (let [half-point (int (/ (count a-seq) 2))]
    (conj [(my-take half-point a-seq)] (my-drop half-point a-seq))))


(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) ()
   (and (not (empty? a-seq)) (or (empty? b-seq) (< (first a-seq) (first b-seq))))
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if
   (< (count a-seq) 2) a-seq
   (let [splitted (halve a-seq)]
     (seq-merge (merge-sort (get splitted 0)) (merge-sort (get splitted 1))))))

(defn monotone [a-seq f]
  (if (< (count a-seq) 2)
    1
  (let [c1 (first a-seq)
        c2 (first (rest a-seq))]
    (if (f c1 c2)
        1
      (inc (monotone (rest a-seq) f))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [biggest (max (monotone a-seq <) (monotone a-seq >))]
      (cons (take biggest a-seq) (split-into-monotonics (drop biggest a-seq))))))

(defn permutations-helper [b-seq n a-seq]
  (if (empty? a-seq)
    (cons b-seq nil)
    (let [perm (permutations-helper (cons (nth a-seq n) b-seq)
                                    0
                                    (concat (take n a-seq) (drop (inc n) a-seq)))]
      (if (== n (dec (count a-seq)))
        perm
        (concat perm (permutations-helper b-seq (inc n) a-seq))))))

(defn permutations [a-set]
   (permutations-helper () 0 (seq a-set)))

(defn powerset-helper [b-set a-set]
  (if (empty? a-set)
    (list b-set)
    (concat (powerset-helper b-set (rest a-set))
            (powerset-helper (conj b-set (first a-set)) (rest a-set)))))

(defn powerset [a-set]
  (powerset-helper #{} a-set))
