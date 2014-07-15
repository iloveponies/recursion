(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [filtered-rest (my-filter pred? (rest a-seq))]
      (if (pred? (first a-seq))
        (cons (first a-seq) filtered-rest)
        filtered-rest))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= (first a-seq) elem)
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (not= (count a-seq) (count b-seq)) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= k 0) 1
   (= k 1) n
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (< n 2) n
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1) ()
   (= how-many-times 1) (list what-to-repeat)
   :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (= up-to 0) ()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (let [c (count a-seq)]
    (map #(drop % a-seq) (range (inc c)))))

(defn inits [a-seq]
  (let [c (count a-seq)]
    (map #(take % a-seq) (range (inc c)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list (list))
    (let [c (count a-seq)]
    (map (fn [shift]
           (concat (drop shift a-seq) (take shift a-seq)))
         (range c)))))

(get {:a 1} :b 0)

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-count (inc (get freqs elem 0))
          new-freqs (assoc freqs elem new-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(first {:a 1})
(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[elem count] (first a-map)]
      (concat (repeat count elem) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [c (count a-seq)
        h (int (/ c 2))]
    [(my-take h a-seq) (my-drop h a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq))
     (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else
     (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (<= (count a-seq) 1)
      a-seq
    :else
      (let [[lhs rhs] (halve a-seq)]
        (seq-merge (merge-sort lhs) (merge-sort rhs)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [is-monotonic? #(or (apply <= %) (apply >= %))
          monotonics (take-while is-monotonic? (rest (inits a-seq)))
          max-monotonic (last monotonics)
          rest-seq (drop (count max-monotonic) a-seq)]
      (cons max-monotonic (split-into-monotonics rest-seq)))))


(defn permutations [a-set]
  (cond
   (= (count a-set) 0)
     '(())
   :else
     (let [rest-perms (permutations (rest a-set))]
       (for [i (range (count a-set))
             r rest-perms]
         (concat (take i r) (list (first a-set)) (drop i r))))))

(defn powerset [a-set]
  (cond
    (empty? a-set)
      #{ #{} }
    :else
      (let [rest-sets           (powerset (rest a-set))
            elem                (first a-set)
            elem-union-fn      #(clojure.set/union % #{elem})
            rest-sets-with-elem (into #{} (map elem-union-fn rest-sets))]
        (clojure.set/union rest-sets rest-sets-with-elem))))
