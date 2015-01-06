(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
   (empty? coll) false
   (not (empty? (rest coll))) false
   :else true))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
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
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)(longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (cons (first a-seq)(my-take-while pred? (rest a-seq)))
   :else '() ))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
 (cond
  (or (and (not (empty? a-seq))(empty? b-seq))
      (and (empty? a-seq)(not (empty? b-seq))))
    false
  (not= (first a-seq)(first b-seq))
    false
  (and (empty? a-seq)(empty? b-seq))
    true
  :else (seq= (rest a-seq)(rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
   (empty? seq-1) seq-1
   (empty? seq-2) seq-2
   :else
     (cons (f (first seq-1)(first seq-2))
           (my-map f (rest seq-1)(rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1)
     '()
   :else
     (conj
        (my-repeat (- how-many-times 1) what-to-repeat)
        what-to-repeat)))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (conj (my-range (- up-to 1)) (- up-to 1))))


(defn tails [a-seq]
 (if (empty? a-seq)
    '(())
    (cons (apply list a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
 (map reverse (reverse (tails (reverse a-seq)))))

(defn rotate [a-seq]
  (concat (rest a-seq) (vector(first a-seq))))

(defn rotations-recursion [rotations-seq a-seq]
  (let [rotated-seq (rotate a-seq)]
    (if (>= (count rotations-seq)(count a-seq))
     rotations-seq
     (rotations-recursion (conj rotations-seq rotated-seq) rotated-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-recursion '() a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [current-elem (first a-seq)]
      (if (contains? freqs current-elem)
        (my-frequencies-helper
           (assoc freqs current-elem (inc (freqs current-elem)))
           (rest a-seq))
        (my-frequencies-helper
           (assoc freqs current-elem 1)
           (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [current (first a-map)]
      (concat (my-repeat (second current) (first current)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (let [times (min n (count coll))]
    (if (zero? times)
      '()
      (cons (first coll) (my-take (dec times) (rest coll))))))

(defn my-drop [n coll]
  (let [times (min n (count coll))]
    (if (zero? times)
      coll
      (my-drop (dec times) (subvec coll 1)))))

(defn halve [a-seq]
  (let [pivot (int (/ (count a-seq) 2))]
   (conj '() (drop pivot a-seq) (take pivot a-seq))))

(defn smaller [a b]
  (cond
   (and (nil? a) (nil? b))
     nil
   (nil? a) b
   (nil? b) a
   :else (min a b)))

(defn seq-merge [a-seq b-seq]
 (let [a (first a-seq)
       b (first b-seq)
       small (smaller a b)
       a-rest (if (= small a)
                 (rest a-seq)
                 a-seq)
       b-rest (if (= small b)
                (rest b-seq)
                b-seq)]
   (if (nil? small)
     '()
     (cons small (seq-merge a-rest b-rest)))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [halved (halve a-seq)
          first-seq (first halved)
          second-seq (second halved)]
      (seq-merge (merge-sort first-seq) (merge-sort second-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
   (let [increasing (last (take-while (fn [x] (if (empty? x)
                       x
                       (or
                        (apply < x)
                        (apply > x))))
               (inits a-seq)))
        inc-len (count increasing)]
    (cons increasing
          (split-into-monotonics (drop inc-len a-seq))))))

(defn permutations-helper [a-set]
  (if (empty? a-set)
    '(())
    (apply concat (map (fn [x]
                         (map (fn [y]
                                (concat (list x) y))
                              (permutations-helper (disj a-set x))))
                       a-set))))

(defn permutations [a-set]
 (permutations-helper (set a-set)))

(defn powerset [a-set]
  [:-])

