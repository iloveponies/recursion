(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if
    (and (not (= nil (not-empty coll)))
         (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (if (or (empty? coll)
          (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if
    (or (empty? a-seq)
        (singleton? a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if
    (or (empty? a-seq)
        (singleton? a-seq))
    (first a-seq)
    (longest-sequence (seq-max
                        (first a-seq)
                        (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (= elem (first a-seq)) true
    (empty? a-seq) false
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= n 1) 1
    :else (+ (fib (dec n))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times)
                                      what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (reverse (map reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq)
                      (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-element (first a-seq)
          new-freqs (if (contains? freqs first-element)
                      (assoc freqs first-element (inc (get freqs first-element)))
                      (assoc freqs first-element 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (concat (repeat (first (vals a-map))
                         (first (keys a-map))))
          (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (== n 0)
          (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (== n 0)
          (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq)
     (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (== 1 (count a-seq)))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic? (fn [seq1] boolean (or (empty? seq1)
                                            (apply < seq1)
                                            (apply > seq1)))
          next-sequence (last (take-while monotonic? (inits a-seq)))]
      (cons next-sequence (split-into-monotonics (drop (count next-sequence)
                                                       a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (mapcat rotations (map #(cons (first a-set) %)
                           (permutations (rest a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [add-element (fn [element comp]
                        (map #(clojure.set/union #{element} %)
                             (powerset comp)))]
      (clojure.set/union (powerset (rest a-set))
                         (add-element (first a-set) (rest a-set))))))

