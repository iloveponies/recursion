(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond (empty? coll) false
        (empty? (rest coll)) true
        :else false))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
      seq-1
      seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (empty? a-seq) (empty? b-seq)
        (empty? b-seq) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) '()
        :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond (= 0 n) 0
        (= 1 n) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [h (first a-seq)
          t (rest a-seq)
          c (or (freqs h) 0)]
      (my-frequencies-helper (assoc freqs h (inc c)) t))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[x c]] (repeat c x)) a-map)))

(defn my-take [n coll]
  (if (or (empty? coll) (<= n 0))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (<= n 0))
    coll
    (my-drop (- n 1) (rest coll))))


(defn halve [a-seq]
  (let [m (quot (count a-seq) 2)]
    [(my-take m a-seq) (my-drop m a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        :else (let [ha (first a-seq)
                    ta (rest a-seq)
                    hb (first b-seq)
                    tb (rest b-seq)]
                (if (<= ha hb)
                  (cons ha (seq-merge ta b-seq))
                  (cons hb (seq-merge a-seq tb))))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[l, r] (halve a-seq)]
      (seq-merge (merge-sort l) (merge-sort r)))))


(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic-up? (fn [s] (= s (sort s)))
          monotonic-down? (fn [s] (= s (reverse (sort s))))
          monotonic? (fn [s] (or (monotonic-up? s) (monotonic-down? s)))
          prefixes (inits a-seq)
          longest-prefix (last (take-while monotonic? (reverse prefixes)))
          suffix (drop (count longest-prefix) a-seq)]
      (cons longest-prefix (split-into-monotonics suffix))))) 

(defn permutations [a-set]
  (cond (empty? a-set) '(())
        (singleton? a-set) [a-set]
        :else (let [insert (fn [v m seq] (apply concat [(my-take m seq)  [v] (my-drop m seq)]))
                    perms (fn [v seq] (map (fn [i] (insert v i seq)) (range 0 (inc (count seq)))))
                    h (first a-set)
                    t (rest a-set)]
                 (apply concat (map (fn [s] (perms h s)) (permutations t))))))

(defn powerset [a-set]
  (cond (empty? a-set) #{#{}}
        :else (let [subsets (map (fn [e] (disj (set a-set) e)) a-set)
                    all (apply concat (map (fn [s] (powerset s)) subsets))]
                (set (concat #{(set a-set)} all #{#{}})))))