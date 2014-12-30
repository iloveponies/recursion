(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
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
   (not (pred? (first a-seq)))
     a-seq
   :else
     (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (== n 0)
     0
   (== n 1)
     1
   :else
     (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (let [d (dec up-to)]
    (if (< d 0)
      '()
      (cons d (my-range d)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [ta (tails a-seq)
        in (reverse (inits a-seq))
        rot (map concat ta in)]
    (if (and (= (last rot) (first rot)) (> (count rot) 1))
      (rest rot)
      rot)))

(defn my-frequencies-helper [freqs a-seq]
  (let [a (first a-seq)
        r (rest a-seq)]
    (cond
     (empty? a-seq)
       freqs
     (contains? freqs a)
       (my-frequencies-helper (assoc freqs a (inc (get freqs a))) r)
     :else
       (my-frequencies-helper (assoc freqs a 1) r))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [f (first a-map)
          fk (first f)
          fv (second f)]
    (concat (repeat fv fk) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (>= 0 n) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (<= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [c (int (/ (count a-seq) 2))]
    [(my-take c a-seq) (my-drop c a-seq)]
  ))

(defn seq-merge [a-seq b-seq]
 (let [fa (first a-seq)
       fb (first b-seq)
       ra (rest a-seq)
       rb (rest b-seq)]
  (cond
   (empty? a-seq)
     b-seq
   (empty? b-seq)
     a-seq
   (< fa fb)
     (cons fa (seq-merge ra b-seq))
   :else
     (cons fb (seq-merge a-seq rb)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
      nil
    (let [in (rest (reverse (inits a-seq)))
          mono? (fn [seq] (or (apply >= seq) (apply <= seq)))
          monos (take-while mono? in)
          big-mono (last monos)
          length (count big-mono)]
      (cons big-mono (split-into-monotonics (my-drop length a-seq))))))

(defn insert [x seq]
  "This produces a series of sequences ofrom the original one (seq) with x inserted into every place possible.
   For exampe (insert x [1 2 3]) => ( (x 1 2 3) (1 x 2 3) (1 2 x 3) (1 2 3 x)).
   Intended to be used as an aid in the permutation function."
  (let [is (reverse (inits seq))
        ts (tails seq)
        xs (repeat (inc (count seq)) x)]
    (map concat is (map cons xs ts))))

(defn permutations [a-set]
  (cond
   (< (count a-set) 1)
     '([])
   (== (count a-set) 1)
     '(a-set)
   (== (count a-set) 2)
     (let [a (first a-set)
           b (last a-set)]
      [[a b] [b a]])
   :else
     (let [a (first a-set)
           end-perms (permutations (rest a-set))
           as (repeat (count end-perms) a)]
            (apply clojure.set/union (map insert as end-perms))))) ;map gives a sequence of sequences for each end permutation, union combines them.


(defn powerset [a-set]
  (if (< (count a-set) 1)
     [[]]
     (let [f (first a-set)
           ps (powerset (rest a-set))
           fs (repeat (count ps) [f])]
       (clojure.set/union ps (map clojure.set/union ps fs)))))
