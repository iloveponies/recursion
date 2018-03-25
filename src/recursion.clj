(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? (rest  coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [seq-1-sz (count seq-1)
        seq-2-sz (count seq-2)]
    (if (> seq-1-sz seq-2-sz)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    :else (seq-max (first a-seq)
                   (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    []
    (if  (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (not (pred? (first a-seq)))
      a-seq
      (my-drop-while pred? (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (not (= (first a-seq) (first b-seq))) false
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (not (empty? a-seq)) (empty? b-seq)) false
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else true))

(defn my-map [f seq-1 seq-2]
  (if (and (first seq-1) (first seq-2))
    (cons (f (first seq-1) (first seq-2)) 
          (my-map f (rest seq-1) (rest seq-2)))
    '()))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (or (zero? n) (= n 1)) n
    (= n 2) 1
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq ())
    (cons (map (fn [x] x)  a-seq) (tails  (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons a-seq ())
    (cons (reverse  (map (fn [x] x) (reverse a-seq))) (inits (reverse (rest (reverse a-seq)))) )))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-freqs (assoc freqs elem (if (contains? freqs elem)
                                        (inc (get freqs elem))
                                        1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies-helper [a-map a-seq]
  (if (empty? a-map)
    (reverse a-seq)
    (let [elem (first (first a-map))
          nr-times (second (first a-map))]
      (un-frequencies-helper (into {} (rest a-map)) (concat (repeat nr-times elem) a-seq)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper a-map '()))

(defn my-take-helper [n coll a-seq]
  (if (empty? coll)
    (reverse  a-seq)
    (if (<= n 0)
      (reverse a-seq)
      (my-take-helper (dec n) (rest coll) (conj  (concat  a-seq) (first coll))))))

(defn my-take [n coll]
  (my-take-helper n coll '()))

(defn my-drop [n coll]
  (if (empty? coll)
    coll
    (if (<= n 0)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [middle (int (/ (count a-seq) 2))]
      (conj [] (my-take middle a-seq) (my-drop middle a-seq)))))


(defn seq-merge-helper [a-seq b-seq merged-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) merged-seq
    (empty? a-seq) (concat merged-seq b-seq)
    (empty? b-seq) (concat merged-seq a-seq)
    :else (if (<= (first a-seq) (first b-seq))
            (seq-merge-helper (rest a-seq) b-seq (reverse (conj (reverse merged-seq) (first a-seq))))
            (seq-merge-helper a-seq (rest b-seq) (reverse (conj (reverse merged-seq) (first b-seq)))))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq '()))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq)) 
    a-seq
    (let [two-halves (halve a-seq)
          lo-half (get two-halves 0)
          hi-half (get two-halves 1)]
      (seq-merge (merge-sort lo-half) (merge-sort hi-half)))))



(defn monotonics-inits-helper [i-seq]
  (if (empty? (rest i-seq))
    (first i-seq)
    (if (and
         (not= (second i-seq) (sort <= (second i-seq)))
         (not= (second i-seq) (sort >= (second i-seq))))
      (first i-seq)
      (monotonics-inits-helper (rest i-seq)))))

(defn split-into-monotonics-helper [a-seq m-seq]
  (if (empty? a-seq)
    (reverse m-seq)
    (let [init-seq (rest (reverse (inits a-seq)))
          init-monotonic (monotonics-inits-helper init-seq)
          nr-monotonic (count init-monotonic)]
      (split-into-monotonics-helper (drop nr-monotonic a-seq) (conj m-seq init-monotonic)))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper a-seq '()))


(defn permutations [a-set])


(defn powerset [a-set])


