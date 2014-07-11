(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not= [] coll) (= [] (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (let [elem (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? elem) (cons elem (my-filter pred? (rest a-seq)))
      :else (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (not (pred? (first a-seq))) []
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
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
    (or (empty? seq-1) (empty? seq-2)) []
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? k) 1
    :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (>= 0 how-many-times) []
    :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (>= 0 up-to) []
    :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) (cons [] a-seq)
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [f (fn [x] (take x a-seq))]
    (map f (range 0 (inc (count a-seq))))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq) [[]]
    :else (take (count a-seq) (iterate (fn [x] (conj (vec (rest x)) (first x))) a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (my-frequencies-helper (assoc freqs (first a-seq) (+ 1 (or (get freqs (first a-seq)) 0))) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) []
    :else (let [[k c] (first a-map)]
      (concat (repeat c k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (= 0 n) nil
    (= nil (first coll)) (my-take (- n 1) (rest coll))
    :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (< 0 n) (my-drop (- n 1) (rest coll))
    :else coll))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (cond
      (>= 1 (count a-seq)) [[] a-seq]
      :else [(my-take mid a-seq) (my-drop mid a-seq)])))

(defn seq-merge [a-seq b-seq]
  (let [afirst (first a-seq)
        bfirst (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) nil
      (empty? a-seq) (cons bfirst (seq-merge a-seq (rest b-seq)))
      (empty? b-seq) (cons afirst (seq-merge (rest a-seq) b-seq))
      (> afirst bfirst) (cons bfirst (seq-merge a-seq (rest b-seq)))
      :else (cons afirst (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (>= 1 (count a-seq)) a-seq
    :else (let [[l r] (halve a-seq)]
      (seq-merge (merge-sort l) (merge-sort r)))))

(defn split-into-monotonics [a-seq]
  (if
    (empty? a-seq) a-seq
    (let [monotonic? (fn [sq] (or (empty? sq) (apply <= sq) (apply >= sq)))
      mono-seq (last (take-while monotonic? (inits a-seq)))]
      (cons mono-seq (split-into-monotonics (drop (count mono-seq) a-seq))))))

(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (empty? (rest a-set)) a-set
    (= 2 (count a-set)) (rotations a-set)
    :else (let [rs (rotations a-set)
                first-and-one (fn [f other] (map (fn [x] (cons f x)) other))]
           (apply concat (map (fn [r] (first-and-one (first r) (permutations (rest r)))) rs)))))

(defn powerset [a-set]
  (map set (cond
            (empty? a-set) '(())
            :else (let [f (first a-set)
              r (rest a-set)]
              (clojure.set/union (powerset r) (map #(conj % f) (powerset r)))))))
