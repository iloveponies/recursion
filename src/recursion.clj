(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and 
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll)) 
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (max-key count seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) ()
    (concat (if (pred? (first a-seq)) (list (first a-seq))) (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (and (not (empty? a-seq)) (or (= (first a-seq) elem) (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    (list)
    (concat (list (first a-seq)) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond  
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (empty? a-seq) (empty? b-seq)) false
    :else (and (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (concat (list (f (first seq-1) (first seq-2))) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (concat (list what-to-repeat) (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (concat (list (dec up-to)) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ())
    (conj (tails (rest a-seq)) (seq a-seq))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (distinct (map concat (tails a-seq) (inits a-seq))))

(defn my-frequencies [a-seq]
  (if (empty? a-seq)
    {}
    (let [prev-freq (my-frequencies (rest a-seq))
          current-element (first a-seq)] 
      (if (contains? prev-freq current-element)
        (assoc prev-freq current-element (inc (get prev-freq current-element)))
        (assoc prev-freq current-element 1)))))


(defn un-frequencies [a-map]
  (apply concat (map repeat (vals a-map) (keys a-map))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
   () 
   (concat (list (first coll)) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [x (int (/ (count a-seq) 2))]
    (list (my-take x a-seq) (my-drop x a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) ()
    (or (empty? b-seq) (and (not (empty? a-seq)) (< (first a-seq) (first b-seq)))) (concat (list (first a-seq)) (seq-merge (rest a-seq) b-seq))
    :else (concat (list (first b-seq)) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    ()
    (let [monotonic-prefixes (take-while #(or (apply <= %) (apply >= %)) (rest (inits a-seq)))]
      (conj (split-into-monotonics (drop (count monotonic-prefixes) a-seq)) (last monotonic-prefixes)))))

(defn separate-nth [a-set n]
  (list (first (my-drop n a-set)) (keep-indexed #(when (not= n %1) %2) a-set)))

(defn multi-conj [seqs elem]
  (map #(conj % elem) seqs))

(defn permutations [a-set]
  (if (or (singleton? a-set) (empty? a-set))
    (list (apply list a-set))
    (mapcat #(multi-conj (permutations (second %)) (first %)) (map #(separate-nth a-set %) (range (count a-set))))))

(defn subset-from-integer [a-set n]
  (keep-indexed #(when (bit-test n %1) %2) a-set))

(defn powerset [a-set]
  (set (map #(subset-from-integer a-set %) (range (power 2 (count a-set))))))
