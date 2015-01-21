(ns recursion)

(defn product [coll]
  (reduce * coll))

(defn singleton? [coll]
  (cond (empty? coll) false
        (empty? (rest coll)) true
        :else false))

(defn my-last [coll]
  (cond (empty? coll) nil
        (empty? (rest coll)) (first coll)
        :else (recur (rest coll))))


(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (reduce max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2 ))

(defn longest-sequence [a-seq]
  (when (seq a-seq)
    (reduce seq-max a-seq)))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (recur pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
    (or (= elem (first a-seq)) (recur elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)]
    (if (or (nil? f) ((complement pred?) f)) '()
      (cons f (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        ((complement pred?) (first a-seq)) a-seq
        :else (recur pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (not= (first a-seq) (first b-seq)) false
        :else (recur (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond (zero? n) 0
        (zero? k) 1
        :else (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (= 1 n) 1
        :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) (list '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq) (list '())
  (let [c (count a-seq)
        bis (concat a-seq a-seq)]
    (map #(take c (drop % bis)) (range c)))))


(defn my-frequencies-helper [freqs a-seq]
  (reduce #(assoc % %2 (if (contains? % %2) (inc (get % %2)) 1)) freqs a-seq))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (mapcat #(repeat (second %) (first %)) (seq a-map)))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (empty? coll) '()
        (zero? n) coll
        :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    (list (take h a-seq) (drop h a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        :else (if (< (first a-seq) (first b-seq))
                (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
                (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq) '()
        (empty? (rest a-seq)) (seq a-seq)
    :else (apply seq-merge (map merge-sort (halve a-seq)))))

(defn monotonic? [a-seq]
  (cond (empty? a-seq) false
        (singleton? a-seq) true
        :else (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (cond (empty? a-seq) '()
        (singleton? a-seq) (list a-seq)
        :else (let [mono (first (filter monotonic? (inits a-seq)))
                    n (count mono)]
                (cons mono (split-into-monotonics (drop n a-seq))))))

(defn permutations [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    (list a-set)
    (mapcat
     #(map (fn [l] (cons (first %) l)) (permutations (rest %)))
     (rotations a-set))))

(defn powerset [a-set]
  (if (empty? a-set) (list '())
    (let [ps (powerset (rest a-set))]
      (clojure.set/union ps
        (map #(cons (first a-set) %) ps)))))



