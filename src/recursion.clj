(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
        (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (or (empty? a-seq) (empty? b-seq))
      (and (empty? a-seq) (empty? b-seq))
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (map reverse (reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)]
      (if (contains? freqs k)
        (my-frequencies-helper
          (assoc freqs k (inc (get freqs k))) (rest a-seq))
        (my-frequencies-helper
          (assoc freqs k 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [a-key (first (first a-map))
          a-val (second (first a-map))]
      (concat (repeat a-val a-key)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (<= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    ()
    (let [a-first (first a-seq)
          b-first (first b-seq)]
      (if (or (nil? b-first)
              (and (not (nil? a-first))
                   (< a-first b-first)))
        (cons a-first (seq-merge (rest a-seq) b-seq))
        (cons b-first (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (nil? (second a-seq)))
    a-seq
    (let [[left right] (halve a-seq)]
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  (loop [acc []
         b-seq a-seq]
    (if (empty? b-seq)
      acc
      (let [monotonic? (fn [pred]
                         (fn [sq]
                           (if (or (empty? sq)
                                   (singleton? sq))
                             true
                             (apply pred sq))))
            inits-seq (inits b-seq)
            inc-seq (longest-sequence
                      (take-while (monotonic? <=)
                                  inits-seq))
            inc-seq-len (count inc-seq)
            dec-seq (longest-sequence
                      (take-while (monotonic? >=)
                                  inits-seq))
            dec-seq-len (count dec-seq)]
        (if (> inc-seq-len
               dec-seq-len)
          (recur (conj acc inc-seq)
                 (drop inc-seq-len b-seq))
          (recur (conj acc dec-seq)
                 (drop dec-seq-len b-seq)))))))

(defn permutations [a-set]
  (cond
    (empty? a-set) (cons () ())
    (= 1 (count a-set)) (list a-set)
    :else (for [a a-set
                b (permutations (disj (set a-set) a))]
            (cons a b))))

(use '[clojure.set :only [union]])
(defn powerset [a-set]
  (apply union #{(set a-set)} (map #(powerset (disj (set a-set) %1)) (set a-set))))

