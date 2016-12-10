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
    (empty? coll)
      nil
    (singleton? coll)
      (first coll)
    :else
      (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
      (first a-seq)
    :else
      (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
          (count seq-2))
     seq-1
     seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= (first a-seq) elem)
      true
    :else
      (recur elem (rest a-seq))))

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
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
   :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1)
      seq-1
    (empty? seq-2)
      seq-2
    :else
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (<= n 0)
      0
    (= n 1)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (find freqs (first a-seq))
      (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [[target times] (first a-map)]
      (concat (repeat times target) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (empty? coll)
      coll
    (= n 0)
      '()
    (> n (count coll))
      coll
    :else
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll)
      coll
    (= n 0)
      coll
    (>= n (count coll))
      '()
    :else
      (cons (get coll n) (my-drop (inc n) coll))))

(defn halve [a-seq]
  (let [ split (int (/ (count a-seq) 2)) ]
    (seq (vector (take split a-seq)
                 (drop split a-seq)))))

(defn seq-merge [a-seq b-seq]
  (let [ helper (fn [a-seq b-seq acc]
                  (cond
                    (empty? a-seq)
                      (concat acc b-seq)
                    (empty? b-seq)
                      (concat acc a-seq)
                    :else
                      (let [seq1 (first a-seq) seq2 (first b-seq)]
                        (if (< seq1 seq2)
                          (recur (rest a-seq) b-seq (concat acc [seq1]))
                          (recur a-seq (rest b-seq) (concat acc [seq2]))))))]
  (helper a-seq b-seq '())))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    (set a-seq)
    (let [monotonic? (fn [a-seq] (or
                                   (apply <= a-seq)
                                   (apply >= a-seq)))
          split-helper (fn [prefix suffix]
                       (if (monotonic? prefix)
                         (list prefix suffix)
                         (recur (butlast prefix)
                                (cons (last prefix)
                                      suffix))))
          [prefix suffix] (split-helper a-seq '())]
      (cons prefix (split-into-monotonics suffix)))))

(defn permutations [a-set]
  (let [perms (fn [[ f & r]]
                 (map (fn [e] (cons f e)) (permutations r)))]
    (cond
      (empty? a-set)
        (cons a-set a-set)
      (= 1 (count a-set))
        (list a-set)
      :else
        (apply concat (map perms (rotations a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    (hash-set (hash-set))
    (clojure.set/union (powerset (next a-set))
           (map #(conj % (first a-set)) (powerset (next a-set))))))

