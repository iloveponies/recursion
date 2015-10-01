(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (= (rest coll) [])
      true
      false)))

(defn my-last [coll]
  (if (not (next coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (not (next a-seq)) (first a-seq)
   :else (max (first a-seq)(max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (cond
   (< (count seq-1) (count seq-2)) seq-2
   (> (count seq-1) (count seq-2)) seq-1
   (= (count seq-1) (count seq-2)) (if (< (max-element seq-1) (max-element seq-2)) seq-2 seq-1)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (not (next a-seq)) (first a-seq)
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
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else (my-take-while pred? '())))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else (cons (first a-seq) (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (and (= (first a-seq) (first b-seq)) (= (count a-seq) (count b-seq))) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (= 0 (count seq-1)) (= 0 (count seq-2))) '()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))


(defn tails [a-seq]
  (if (= 0 (count a-seq))
    (cons a-seq '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (= 0 (count a-seq))
    (cons a-seq '())
    (reverse (map reverse (tails (reverse a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          sub-freqs (if (contains? freqs elem)
                      (assoc freqs elem (inc (freqs elem)))
                      (assoc freqs elem 1))]
      (my-frequencies-helper sub-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [[first-amount] (vals a-map)
        [first-key] (keys a-map)]
    (if (empty? a-map)
      []
      (concat (repeat first-amount first-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (= 0 (count coll)))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (= n 0) coll
    (= (count coll) 0) '()
    :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [seqfirst (int (/ (count a-seq) 2))]
    (vector (my-take seqfirst a-seq) (my-drop seqfirst a-seq))))


(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    []
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= (first a-seq) (first b-seq))
       (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     :else
       (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [divided-seq (halve a-seq)
          seq-1 (first divided-seq)
          seq-2 (second divided-seq)]
      (seq-merge (merge-sort seq-1) (merge-sort seq-2)))))

(defn is-monotonic [initial]
  (or (empty? initial) (apply <= initial) (apply >= initial)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonics (take-while is-monotonic (inits a-seq))
          longest-monotonic (last monotonics)
          rest (drop (count longest-monotonic) a-seq)]
      (cons longest-monotonic (split-into-monotonics rest)))))


(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (singleton? a-set) (list (apply list a-set))
    :else (let [drop-nth (fn [n] (concat (take n a-set) (drop (inc n) a-set)))
                complements-of-a-set (map drop-nth (range (count a-set)))
                sub-perms (map permutations complements-of-a-set)]
            (apply concat
              (map (fn [a a-sub-perms]
                (map (fn [a-sub-perm]
                  (cons a a-sub-perm)) a-sub-perms)) a-set sub-perms)))))

(defn powerset [a-set]
  (cond
    (empty? a-set) #{#{}}
    :else (let [super-set (set a-set)
                subsets (map (fn [a] (disj super-set a)) super-set)]
            (conj (apply clojure.set/union (map powerset subsets)) super-set))))

