(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (not (empty? (rest coll))) (my-last (rest coll))
    :else (first coll)))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
    (cond
      (== len1 len2) seq-2
      (< len1 len2) seq-2
      :else seq-1)))

;; TODO: get rid of sort-by
(defn longest-sequence [s]
  (let [len (count s)]
    (cond
      (== 0 len) nil
      (== 1 len) (first s)
      :else (last (sort-by count s)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) ()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (let [a (count a-seq)
        b (count b-seq)]
    (cond
      (not (== a b)) false
      (and (== 0 a) (== 0 b)) true
      (== 1 a b) (= (first a-seq) (first b-seq))
      (not (= (first a-seq) (first b-seq))) false
      :else (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (== 0 n k) nil ;; not defined
    (zero? n) 0
    (and (pos? n) (zero? k)) 1
    (and (neg? n) (zero? k)) -1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (< 0 n 3) 1
    :else (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [times e]
  (cond
     (<= times 0) ()
     :else (cons e (my-repeat (dec times) e))))

(defn my-range [up-to]
  (cond
    (<= up-to 0) ()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
   (cond
     (empty? a-seq) (conj a-seq ())
     :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rot-helper [a-seq n]
  "A rotation is the first element removed and appended to the rest of the seq"
  (let [rot (concat (rest a-seq) [(first a-seq)])]
    (cond
      (== n 0) ()
      :else (cons rot (rot-helper rot (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
    (rot-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (let [k (first a-seq)
          v (get freqs k 0)]
      (my-frequencies-helper (assoc freqs k (inc v)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) ()
    (let [e (first (first a-map))
          n (second (first a-map))]
      (concat (repeat n e) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (< n 1) ()
    (nil? (first coll)) ()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (< n 1) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    (vector (take middle a-seq) (drop middle a-seq))))

(defn smaller [a b]
  (cond
    (and (not (number? a)) (not (number? b))) nil
    (not (number? a)) b
    (not (number? b)) a
    :else (min a b)))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq) b (first b-seq)]
    (cond
      (and (nil? a) (nil? b)) ()
      (nil? a) (cons b (seq-merge a-seq (rest b-seq)))
      (nil? b) (cons a (seq-merge (rest a-seq) b-seq))
      (<= a b) (cons a (seq-merge (rest a-seq) b-seq))
      :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1) a-seq
    (let [puoli (halve a-seq)]
      (seq-merge
        (merge-sort (first puoli))
        (merge-sort (second puoli))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

