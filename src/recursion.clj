(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (if (zero? (first coll))
      0
      (* (first coll) (product (rest coll))))))

(defn product2 [coll]
  (apply * coll))

(defn singleton? [coll]
  (if (or (empty? coll) (not (empty? (rest coll))))
    false
    true))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn max-element-2 [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (> count-1 count-2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

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
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (count a-seq) (count b-seq)) false
    (not= (first a-seq) (first b-seq))false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      '()
    :else
      (cons
        (f (first seq-1) (first seq-2))
        (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (zero? k) 1
    :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (>= 0 how-many-times) '()
    :else (cons
            what-to-repeat
            (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (>= 0 up-to) `()
    :else (cons
            (- up-to 1)
            (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) `(())
    :else (cons (sequence a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
    (empty? a-seq) `(())
    :else (cons (sequence a-seq) (inits (pop a-seq)))))

(defn rotations [a-seq]
  (let [t (tails a-seq)
        i (reverse (inits a-seq))]
    (cond (empty? a-seq) `(())
          :else (rest (map concat t i)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [next-key (first a-seq)
           new-freq (assoc
                     freqs
                     next-key
                     (inc (get freqs next-key 0)))]
      (my-frequencies-helper new-freq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [n (last (first a-map))
          x (first (first a-map))
          new-seq (concat a-seq (repeat n x))]
      (un-frequencies-helper new-seq (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take-helper [new-coll n coll]
  (if (or (zero? n) (empty? coll))
    new-coll
    (my-take-helper (conj new-coll (first coll)) (dec n) (rest coll))))
(defn my-take [n coll]
  (my-take-helper [] n coll))

(defn my-drop-helper [n coll]
  [:-])

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge-helper [final-seq a-seq b-seq]
  (cond
    (empty? a-seq) (concat final-seq b-seq)
    (empty? b-seq) (concat final-seq a-seq)
    :else (let [a (first a-seq)
                b (first b-seq)]
            (cond
              (> a b) (seq-merge-helper (merge final-seq b) a-seq (rest b-seq))
              :else (seq-merge-helper (merge final-seq a) (rest a-seq) b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (let [[x y] (halve a-seq)
        x-sorted (cond (> 2 (count x)) x :else (merge-sort x))
        y-sorted (cond (> 2 (count y)) y :else (merge-sort y))]
    (seq-merge x-sorted y-sorted)))

(defn add-to-last [result x]
  (conj (vec (drop-last result)) (vec (conj (last result) x))))
(defn add-to-new [result x]
  (conj result [x]))
(defn m-helper [result a-seq]
  (cond
    (empty? a-seq) result
    (or
      (= 1 (count (last result)))
      (or
        (> (first a-seq) (last (last result)) (last (drop-last (last result))))
        (< (first a-seq) (last (last result)) (last (drop-last (last result))))))
          (m-helper
            (add-to-last result (first a-seq))
            (vec (rest a-seq)))
    :else (m-helper
            (add-to-new result (first a-seq))
            (vec (rest a-seq)))))
(defn split-into-monotonics [a-seq]
  (m-helper [[(first a-seq)]] (vec (rest a-seq))))

; --- permutations ---
(defn v-insert [vec index x]
  (concat (take index vec) [x] (drop index vec)))

(defn p-for-one [old x]
  (for [i (range 0 (inc (count old)))]
    (v-insert (vec old) i x)))

(defn p-for-many [old x]
  (apply concat (for [p old] (p-for-one p x))))

(defn p-helper [result index a-set]
  (cond
    (= index (count a-set)) result
    :else (p-helper
            (p-for-many result (nth (vec a-set) index))
            (inc index)
            a-set)))

(defn permutations [a-set]
  (p-helper (list '()) 0 a-set))

; --- powerset ---
(defn exp [x n]
  (reduce * (repeat n x)))

(defn base2 [x]
  (Integer/toString x 2))

(defn pad0 [x length]
  (clojure.string/replace (format (str "%" length "s") x) #" " "0"))

(defn p-set-in-base2 [a-set]
  (map
    (fn [x] (pad0 (base2 x) (count a-set)))
    (range 0 (exp 2 (count a-set)))))

(defn replce-helper [result base2-string a-set]
  (cond
    (= 0 (count a-set)) result
    (= \0 (first base2-string)) (replce-helper result (rest base2-string) (rest a-set))
    :else (replce-helper (conj result (first a-set)) (rest base2-string) (rest a-set))))

(defn replace [base2-string a-set]
  (replce-helper #{} base2-string a-set))

(defn powerset [a-set]
  (cond
    (empty? a-set) #{a-set}
    :else (let [base2-p-set (p-set-in-base2 a-set)]
            (map #(replace % a-set) base2-p-set))))


