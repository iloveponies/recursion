(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [my-cons (fn [x]
                  (if (pred? x)
                    (cons (first a-seq) (my-filter pred? (rest a-seq)))
                    (my-filter pred? (rest a-seq))))]
    (if (empty? a-seq)
      a-seq
      (my-cons (first a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
   (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)) 
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (zero? n)
    0
    (if (= n 1)
      1
      (+ (fib (- n 2)) (fib (- n 1))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< (dec up-to) 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (seq (tails (rest a-seq))))))

(defn inits [a-seq]
   (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (seq (inits (reverse (rest (reverse a-seq))))))))

(defn rotations [a-seq]
  (let [my-get (fn [x] (get a-seq x))
        tail-x (fn [x] (map my-get (range x (count a-seq))))
        head-x (fn [x] (map my-get (range 0 x)))] 
    (if (empty? a-seq)
      '(())
      (map (fn [x] (concat (tail-x x) (head-x x)))
         (range 0 (count a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper
       (conj freqs {(first a-seq) (inc (get freqs (first a-seq)))})
       (rest a-seq))
      (my-frequencies-helper
       (conj freqs {(first a-seq) 1})
       (rest a-seq)))))

(defn my-frequencies [a-seq]
   (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [f (fn [x] (my-repeat (last x) (first x)))]
    (apply concat (map f a-map))))

(defn my-take [n coll]
  (let [elem-at (fn [x] (get coll x))]
    (if (> n (count coll))
      (seq coll)
      (map elem-at (range 0 n)))))

(defn my-drop [n coll]
  (let [my-get (fn [x] (get coll x))
        tail-x (fn [x] (map my-get (range x (count coll))))]
    (cond
      (>= n (count coll)) '()
      (<= n 0) (seq coll)
      :else (seq (tail-x n)))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (take half a-seq)
            (drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (nil? (first a-seq)) b-seq
    (nil? (first b-seq)) a-seq
    :else (if (<= (first a-seq) (first b-seq))
            (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
            (cons (first b-seq) (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [h (halve a-seq)
          l-side (first h)
          r-side (last h)]
      (seq-merge (merge-sort l-side) (merge-sort r-side)))))

(defn split-into-monotonics [a-seq]
  (let [l (take (count (take-while
                        (fn [x] (or (< (count x) 2)
                                    (apply < x)
                                    (apply > x)))
                        (rest (reverse (inits a-seq))))) a-seq)]
    (cond
      (< (count a-seq) 2) a-seq 
      :else (cons l (split-into-monotonics (drop (count l) a-seq))))))

(defn permutations [a-set]
  (let [elems (set a-set)]
   (cond 
     (empty? elems) (list ())
     :else (for [c elems
                 r (permutations (disj elems c))]
             (cons c r)))))

(defn powerset [a-set])
