(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (= false (pred? (first a-seq)))
      (my-filter pred? (rest a-seq))
      (cons (first a-seq)
          (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not (= elem (first a-seq))) (sequence-contains? elem (rest a-seq))
   :else true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (= n 0)
    0
    (if (= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

; dec n -> (- n 1)

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
   (if (empty? a-seq)
     '([])
     (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
    (if (empty? a-seq)
    [a-seq]
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
      freqs
      (let [elem (first a-seq)
            new-freqs (if (contains? freqs elem)
                      (assoc freqs elem (inc (get freqs elem)))
                      (assoc freqs elem 1))]
        (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [x] (repeat (get a-map x) x)) (keys a-map))))

(defn my-take [n coll]
  (if (or (empty? coll) (>= 0 n))
      '()
      (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [half-elements (int (/ (count a-seq) 2))]
    (vector (my-take half-elements a-seq) (my-drop half-elements a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) '()
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a b) (cons a (seq-merge (rest a-seq) b-seq))
      :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic? (fn [x] (or (apply < x) (apply >= x)))
          candidates (drop 1 (reverse (inits a-seq)))
          smallest-monotonic (last (take-while monotonic? candidates))
          rest-of-the-candidates (drop (count smallest-monotonic) a-seq)]
      (cons smallest-monotonic (split-into-monotonics rest-of-the-candidates)))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (mapcat
      (fn [x] (map (fn [y] (cons x y)) (permutations (remove (partial = x) a-set)))) a-set)))

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
      (let [rest-of-the-sets (powerset (rest a-set))
            combined-sets (map (fn [x] (conj x (first a-set))) rest-of-the-sets)]
        (clojure.set/union combined-sets rest-of-the-sets))))

