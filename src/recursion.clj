(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [a (first a-seq)]
    (if (empty? (rest a-seq))
      (first a-seq)
      (max a (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (>= (count seq-2) (count seq-1))
    (seq seq-2)
    (seq seq-1)))

(defn longest-sequence [a-seq]
  (let [a (first a-seq)]
    (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max a (longest-sequence (rest a-seq))))))

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
    (empty? a-seq) []
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (not= (count a-seq) (count b-seq)) false
    (empty? a-seq) (if (empty? b-seq) true false)
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    ))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) []
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
   (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= how-many-times 1)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (>= up-to 1)
    (cons (dec up-to) (my-range (dec up-to)))
    '()))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (set (map concat (tails a-seq) (reverse (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                      (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (my-repeat (last (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (>= n (count coll))
    coll
    (if (== n 0)
      '()
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (== n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (list (my-take n a-seq) (my-drop n a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (if (> (first a-seq) (first b-seq))
      (concat [(first b-seq)] (seq-merge a-seq (rest b-seq)))
      (concat [(first a-seq)] (seq-merge (rest a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (last (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    nil
    (let [monotonics (take-while (fn [x] (or (seq= x (sort x)) (seq= x (reverse (sort x))))) (reverse (inits a-seq)))]
      (cons (last monotonics) (split-into-monotonics (drop (dec (count monotonics)) a-seq))))))


(defn permutations [a-set]
  (if (empty? a-set)
    (list [])
    (let [my-concat (fn [x] (concat x [(first a-set)]))]
      (set (mapcat rotations (cons (seq a-set) (map my-concat (permutations (rest a-set)))))))))

(defn ps1 [a-set]
  (cons a-set (map set (mapcat (fn [x] (ps1 (remove #{x} a-set))) a-set))))

(defn powerset [a-set]
  (set (ps1 (set a-set))))
