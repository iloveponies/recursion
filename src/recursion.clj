(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (= (first a-seq) elem) true
   (or (singleton? a-seq) (empty? a-seq)) false
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    ()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (or (empty? a-seq) (empty? b-seq))
     (if (and (empty? a-seq) (empty? b-seq)) true false)
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) ()
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= k 0) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (= 0 up-to) ()
   :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [a (inits a-seq)
      b (reverse (tails a-seq))
      ans (map concat b a)]
  (if (> (count ans) 1)
    (rest ans)
    ans)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          count-elem (get freqs elem)]
      (if (nil? (get freqs elem))
      (my-frequencies-helper (assoc freqs elem 1) (rest a-seq))
      (my-frequencies-helper (assoc freqs elem (+ count-elem 1)) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [keys a-map]
  (if (empty? keys)
    []
    (let [elem (first keys)
          elem-count (get a-map elem)]
      (concat (repeat elem-count elem) (un-frequencies-helper (rest keys) a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper (keys a-map) a-map))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    '()
    (conj (my-take (- n 1) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (= n 1) (rest coll)
   :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [halfp (int (/ (count a-seq) 2))]
    (if (= halfp 0)
      [() a-seq]
      [(my-take halfp a-seq) (my-drop halfp a-seq)])))

(defn seq-merge-helper [a-seq b-seq]
  (let [take-a-seq (fn [a-seq b-seq] (conj (seq-merge-helper a-seq (rest b-seq)) (first b-seq)))
        take-b-seq (fn [a-seq b-seq] (conj (seq-merge-helper (rest a-seq) b-seq) (first a-seq)))]
    (cond
     (and (empty? a-seq) (empty? b-seq)) nil
     (empty? a-seq) (take-a-seq a-seq b-seq)
     (empty? b-seq) (take-b-seq a-seq b-seq)
     (< (first a-seq) (first b-seq)) (take-b-seq a-seq b-seq)
     :else (take-a-seq a-seq b-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics-helper [prev-num current-list current-addable]
  (if (empty? current-list)
    [current-addable]
  (let [current-num (first current-list)
        test (conj current-addable current-num)]
    (if (or (apply <= test) (apply >= test))
      (split-into-monotonics-helper current-num (rest current-list) (conj current-addable current-num))
     (conj (split-into-monotonics-helper current-num (rest current-list) [current-num]) current-addable)))))

(defn split-into-monotonics [a-seq]
  (reverse (split-into-monotonics-helper (first a-seq) (rest a-seq) [(first a-seq)])))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

