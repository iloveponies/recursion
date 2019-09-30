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
    (empty? coll)     nil
    (singleton? coll) (first coll)
    :else             (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)
      nil
    (empty? (rest a-seq))
      (first a-seq)
    :else
      (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq)) 
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
    :else 
      (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= elem (first a-seq)) true
    :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq)) 
      (cons (first a-seq) 
            (my-take-while pred? (rest a-seq)))
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
    (and (empty? a-seq) (empty? b-seq)) 
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) 
      seq-1
    (empty? seq-2) 
      seq-2
    :else
      (cons 
        (f (first seq-1) (first seq-2)) 
        (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k) 
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          elem-count (if (get freqs elem)
                       (inc (get freqs elem))
                       1)
          new-freqs (assoc freqs elem elem-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper '{} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [elem (key (first a-map))
          elem-count (val (first a-map))
          new-seq (conj a-seq elem)
          new-map (if (<= elem-count 1)
                    (dissoc a-map elem)
                    (assoc a-map elem (dec elem-count)))]
      (un-frequencies-helper new-seq new-map))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (<= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))
        first-half (my-take half a-seq)
        second-half (my-drop half a-seq)]
    [first-half second-half]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) 
      '()
    (empty? a-seq)
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    (empty? b-seq)
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn take-while-leq-helper [take-n a-seq]
  (if (or (empty? a-seq) (= take-n (count a-seq)))
    a-seq
    (if (apply <= (take take-n a-seq))
      (take-while-leq-helper (inc take-n) a-seq)
      (take (dec take-n) a-seq))))

(defn take-while-leq [a-seq]
  (take-while-leq-helper 1 a-seq))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [mono (take-while-leq a-seq)
          rest-seq (drop (count mono) a-seq)]
      (cons mono (split-into-monotonics rest-seq)))))

(defn into-seq-helper [x f-set b-set]
  (if (empty? b-set)
    (cons (concat f-set (cons x b-set)) '())
    (cons 
      (concat f-set (cons x b-set))
      (into-seq-helper x (conj f-set (first b-set)) (rest b-set)))))

(defn into-seq [x a-set]
  (into-seq-helper x '() a-set))

(defn permutations [a-set]
  (cond
    (empty? a-set)
      '(())
    (empty? (rest a-set))
      (cons a-set '())
    :else
      (let [into-seq-x (fn [x-seq] (into-seq (first a-set) x-seq))]
        (apply concat (map into-seq-x (permutations (rest a-set)))))))


(defn powerset [a-set]
  (if (empty? a-set) '(())
    (clojure.set/union (powerset (next a-set))
           (map #(conj % (first a-set)) (powerset (next a-set))))))

