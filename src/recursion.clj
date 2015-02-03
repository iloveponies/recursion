(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '[]))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (empty? a-seq) (empty? b-seq)
   (empty? b-seq) (empty? a-seq)
   :else          (if (= (first a-seq) (first b-seq))
                    (seq= (rest a-seq) (rest b-seq))
                    false)))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) ()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k) 1
   (zero? n) 0
   :else (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) ()
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (zero? up-to) ()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let
    [new-head (fn [head tail] (concat head [(first tail)]))
     rotator (fn rotator [head tail]
               (cond
                (empty? tail) '()
                :else (cons
                       (concat tail head)
                       (rotator (new-head head tail) (rest tail)))))]
    (cond
     (empty? a-seq) '(())
     :else (rotator '() a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let
    [element (first a-seq)
     update-freqs (fn [freqs key]
                    (assoc freqs element (inc (if (get freqs element)
                                                (get freqs element)
                                                0))))]
    (cond
     (empty? a-seq) freqs
     :else (my-frequencies-helper
            (update-freqs freqs element)
            (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
   (empty? a-map) a-map
   :else (let [current-key (key (first a-map))
               current-value (val (first a-map))]
           (concat
            (repeat current-value current-key)
            (un-frequencies (rest a-map)) ))))

(defn my-take [n coll]
  (cond
   (= 0 n) ()
   (empty? coll) ()
   :else (cons  (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) coll
   (> n 0) (my-drop (dec n) (rest coll))
   :else coll))

(defn halve [a-seq]
  (let [half-index (int (/ (count a-seq) 2))]
    [(my-take half-index a-seq) (my-drop half-index a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (not (empty? b-seq))) b-seq
   (and (empty? b-seq) (not (empty? a-seq))) a-seq
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [[first-half second-half] (halve a-seq)]
    (cond
     (or (empty? a-seq) (empty? (rest a-seq))) a-seq
     :else (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn monotonic? [a-seq]
  (cond
   (empty? a-seq) true
   :else (or (apply <= a-seq) (apply >= a-seq))))

(defn monotonic-until [inits-seq & [n]]
  (let [counter (or n 0)]
    (cond
     (empty? inits-seq) counter
     (monotonic? (first inits-seq)) (monotonic-until
                                     (rest inits-seq)
                                     (inc counter))
     :else counter)))

(defn split-into-monotonics [a-seq]
  (let [inits-seq (rest (inits a-seq))]
    (cond
     (empty? a-seq) '()
     :else (cons
            (take (monotonic-until inits-seq) a-seq)
            (split-into-monotonics (drop (monotonic-until inits-seq) a-seq))))))

(defn find-ascending-pair-index [v & [i]]
  (if (empty? v)
    nil
    (let [index (or i (dec (count v)))]
      (cond
       (= index 0) nil
       (< (v (dec index)) (v index )) (dec index)
       :else (find-ascending-pair-index v (dec index))))))

(defn find-last-greater-than-index [v index & [i]]
  (let [reference (v index)
        try-index (or i (dec (count v)))]
    (cond
     (< reference (v try-index)) try-index
     :else (find-last-greater-than-index v index (dec try-index)))))

(defn swap [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))

(defn reverse-from-index [v index]
  (vec (concat (subvec v 0 index) (reverse (subvec v index)))))

(defn perms-helper [v]
  (cond
   (empty? v) '(())
   (empty? (rest v)) '(())
   :else (let [asc-pair-idx (find-ascending-pair-index v)]
           (if (not asc-pair-idx)
             (cons (seq v) '())
             (let [lgt-idx (find-last-greater-than-index v asc-pair-idx)]
               (cons
                (seq v)
                (perms-helper (reverse-from-index
                               (swap v asc-pair-idx lgt-idx)
                               (inc asc-pair-idx)))))))))

(defn permutations
  "Finds all permutations of a set based on lexicographic order.
  See http://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order"
  [a-set]
  (perms-helper (vec (merge-sort a-set))))

(defn add-element-to-set
  "Returns the given set with the given element added."
  [a-set element]
  (set(cons element a-set)))

(defn powerset
  "The power set of the empty set is the set containing the
  empty set and the power set of any other set is all the subsets of the
  set containing some specific element and all the subsets of the set not
  containing that specific element."
  [a-set]
  (let [subsets-without-first-element (fn [a-set] (powerset (rest a-set)))
        subsets-with-first-element (fn [a-set] (map
                                                (fn [subset] (add-element-to-set subset (first a-set)))
                                                (powerset (rest a-set))))]
    (cond
     (empty? a-set) #{#{}}
     :else (clojure.set/union
            (subsets-without-first-element a-set)
            (subsets-with-first-element a-set)))))
