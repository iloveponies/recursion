(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (empty? (rest a-seq))
      (first a-seq)
    :else
      (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)
          r (my-filter pred? (rest a-seq))]
      (if (pred? f) (cons f r) r))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (== (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if
    (or (empty? seq-1) (empty? seq-2)) []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec (dec n))) (fib (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    []
    (let [n (dec up-to)]
      (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-count (if (contains? freqs elem)
                      (inc (get freqs elem))
                      1)
          new-freqs (assoc freqs elem new-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

;(defn un-frequencies [a-map]
;  (apply concat (map repeat (vals a-map) (keys a-map))))
(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[val num] (first a-map)]
      (concat (repeat num val) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a (first a-seq)
                b (first b-seq)]
            (if (< a b)
              (cons a (seq-merge (rest a-seq) b-seq))
              (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[seq1 seq2] (halve a-seq)]
      (seq-merge (merge-sort seq1) (merge-sort seq2)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [init-list (rest (inits a-seq))
          my-sorted? (fn [s] (let [ss (sort s)] (or (seq= ss s) (seq= ss (reverse s)))))
          num-sorted (count (take-while my-sorted? init-list))
          mono (take num-sorted a-seq)]
      (cons (take num-sorted a-seq) (split-into-monotonics (drop num-sorted a-seq))))))

(defn permutations [a-seq]
  (if (empty? a-seq)
    [[]]
    (let [rest-perms (permutations (rest a-seq))
          mapped (map #(cons (first a-seq) %) rest-perms)]
      (mapcat rotations mapped))))


(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [rest-pset (powerset (rest a-set))
          joined (set (map #(conj % (first a-set)) rest-pset))]
      (clojure.set/union rest-pset joined))))


