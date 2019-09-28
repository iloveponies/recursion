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
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element(rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence(rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (== elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))


(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [seq-1 seq-2]
  (if (and (empty? seq-1) (empty? seq-2))
    true
    (if (and (first seq-1) (first seq-2))
      (if (= (first seq-1) (first seq-2))
        (seq= (rest seq-1) (rest seq-2))
        false
      )
      false
    )
  ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))
  ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
  ))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))
  ))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))
  ))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (inits (butlast a-seq)))
  ))

(defn rotations-helper [n a-seq]
  (let [rotated (concat (rest a-seq) (vector(first a-seq)))]
    ;(cons a-seq [rotated])))
    (if (not= n 0)
      (cons a-seq (rotations-helper (dec n) rotated)))
  ))

(defn rotations [a-seq]
  (let [size-of-a-seq (count a-seq)]
    ;size-of-a-seq))
    (if (= size-of-a-seq 0)
      [[]]
      (rotations-helper size-of-a-seq a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [a-key (first a-seq)
        a-value (get freqs a-key)]
    (if (empty? a-seq)
      freqs
      ; if first element is already a key in freqs
      (if a-value
        ; increment the value in freqs
        (my-frequencies-helper (assoc freqs a-key (inc a-value)) (rest a-seq))
        ; else set to initial value
        (my-frequencies-helper (assoc freqs a-key 1) (rest a-seq))
  ))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [a-key (first(first a-map))
        a-value (last(first a-map))]
    (if (empty? a-map)
      ()
      (concat (repeat a-value a-key) (un-frequencies (rest a-map)))
  )))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))
  ))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (dec n) (rest coll))
  ))

(defn halve [a-seq]
  (let [split (int(/ (count a-seq) 2))]
    [(my-take split (seq a-seq)) (my-drop split (seq a-seq))]
  ))

(defn seq-merge [a-seq b-seq]
  (let [first-a-seq (first a-seq)
        first-b-seq (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) ()
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      :else
        (if (<= first-a-seq first-b-seq)
          (cons first-a-seq (seq-merge (rest a-seq) b-seq))
          (cons first-b-seq (seq-merge a-seq (rest b-seq)))
    ))))

(defn merge-sort [a-seq]
  (let [a-seq-count (count a-seq)
        first-half (first(halve a-seq))
        second-half (second(halve a-seq))]
    (if (or (= a-seq-count 0) (= a-seq-count 1))
      a-seq
      (seq-merge (merge-sort first-half) (merge-sort second-half))
  )))

(defn ascending-monotonics? [a-seq]
  (cond
    (empty? a-seq) true
    (= (count a-seq) 1) true
    :else (<= (first a-seq) (second a-seq))
   ))

(defn split-into-monotonics-helper [ascending a-seq]
  (loop [result []
         monotonics []
         asc ascending
         a-seqe a-seq]
    (let [current-e (first a-seqe)
          next-e (second a-seqe)]
      (if (empty? a-seqe)
        (conj result monotonics)
        (cond
          asc   (if (or (= next-e nil) (<= current-e next-e))
                  (recur result (conj monotonics current-e) asc (rest a-seqe))
                  (recur (conj result (conj monotonics current-e)) [] (ascending-monotonics? (rest a-seqe)) (rest a-seqe)))
          :else (if (or (= next-e nil) (>= current-e next-e))
                  (recur result (conj monotonics current-e) asc (rest a-seqe))
                  (recur (conj result (conj monotonics current-e)) [] (ascending-monotonics? (rest a-seqe)) (rest a-seqe)))
         )))))

(defn split-into-monotonics [a-seq]
  (cond
    (empty? a-seq) []
    (= (count a-seq) 1) [(first a-seq)]
    :else (split-into-monotonics-helper (ascending-monotonics? a-seq) a-seq)
   ))

(defn swap [a-seq i j]
  (let [at-i (get a-seq i)
        at-j (get a-seq j)]
    (assoc (assoc a-seq i at-j) j at-i)))

;To shuffle an array a of n elements (indices 0..n-1):
;  for i from n − 1 downto 1 do
;       j ← random integer with 0 ≤ j ≤ i
;       exchange a[j] and a[i]
(defn random-permutation [a-seq]
  (loop [i (dec (count a-seq))
         permutation a-seq]
    (if (= i 0)
      permutation
      (recur (dec i) (swap permutation i (rand-int (inc i)))))))

(defn contains-seq? [a-seq-of-seq a-seq]
  (loop [i 0
         rest-of-a-seq-of-seq a-seq-of-seq]
    (if (= i (count a-seq-of-seq))
      false
      (if (= (first rest-of-a-seq-of-seq) a-seq)
        true
        (recur (inc i) (rest rest-of-a-seq-of-seq))))))

(defn valid-permutation [permutations a-seq]
  (loop [permutation (random-permutation a-seq)]
    (if (contains-seq? permutations permutation)
      (recur (random-permutation a-seq))
      permutation)))

(defn random-permutations [a-seq permutations-count]
  (loop [i 0
         permutations []]
    (if (= i permutations-count)
      permutations
      (recur (inc i) (conj permutations (valid-permutation permutations a-seq))))))

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn permutations [a-set]
  (if (or (empty? a-set) (= (count a-set) 1))
    [(vec a-set)]
    (random-permutations (vec a-set) (factorial (count a-set)))))

(defn exp [x n]
  (reduce * (repeat n x)))

(defn two-exponents [es]
  (reduce (fn [results e] (conj results (exp 2 e))) [] es))

(defn exp-map [a-set exps]
  (if (not= (count a-set) (count exps))
    nil
    (loop [i (- (count a-set) 1)
           a-map {}]
      (if (= i -1)
        a-map 
        (recur (dec i) (assoc a-map (get exps i) (get a-set i)))))))

(defn a-powerset [powerset-value powerset-map]
  (reduce (fn [results key-val] 
            (if (> (bit-and powerset-value (get key-val 0)) 0)
              (conj results (get key-val 1))
              results))
          #{} powerset-map))
  
(defn powerset [a-set]
  (let [powerset-map (exp-map (vec a-set) (two-exponents (range (count (vec a-set)))))]
    (loop [i 0
           powersets #{}]
      (if (= i (exp 2 (count (vec a-set))))
        powersets
        (if (= i 0)
          (recur (inc i) (conj powersets #{}))
          (recur (inc i) (conj powersets (a-powerset i powerset-map))))))))

