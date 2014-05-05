(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll)) (first coll) (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq) (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
    (if (= elem (first a-seq)) true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) '()
    (if (not (pred? (first a-seq))) '()
      (cons (first a-seq) (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) '()
    (if (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (let [one-is-empty? (or (and (empty? a-seq) (not (empty? b-seq)))
                          (and (not (empty? a-seq)) (empty? b-seq)))]
    (if (and (empty? a-seq) (empty? b-seq)) true
      (if one-is-empty? false
        (if (not= (first a-seq) (first b-seq)) false
          (seq= (rest a-seq) (rest b-seq)))))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0) 1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (= n 0) 0
    (if (= n 1) 1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) '([])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq) (cons '() '())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq) (cons '() '())
    (let [b-seq (concat a-seq a-seq)
          l (count a-seq)]
      ((fn f [f-seq] (if (<= (count f-seq) l) nil
                     (cons (take l f-seq) (f (rest f-seq))))) b-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (my-frequencies-helper
     (assoc freqs
       (first a-seq)
       (+ (if (contains? freqs (first a-seq)) (get freqs (first a-seq)) 0) 1))
     (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
    (let [frst (first a-map)]
      (concat (repeat (second frst) (first frst)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll)) '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (< n 1) coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [hlf (int (/ (count a-seq) 2))]
    [(my-take hlf a-seq) (my-drop hlf a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq) b-seq
    (if (empty? b-seq) a-seq
      (if (< (first a-seq) (first b-seq))
        (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2) a-seq
    (let [halved (halve a-seq)]
      (seq-merge (merge-sort (first halved)) (merge-sort (second halved))))))

(defn monotonic-seq [tyyppi prev a-seq]
  (if (empty? a-seq) '()
    (if (tyyppi prev (first a-seq))
      (cons (first a-seq) (monotonic-seq tyyppi (first a-seq) (rest a-seq))) '())))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) '()
    (let [tyyppi (if (>= (first a-seq) (second a-seq)) >= <=)
          first-monotonic (monotonic-seq tyyppi (first a-seq) a-seq)]
      ;first-monotonic)))
      (cons first-monotonic (split-into-monotonics (drop (count first-monotonic) a-seq))))))

(defn prefix [x a-seq]
  (map (fn [y] (cons x y)) a-seq))

(defn permutations [a-set]
  (if (< (count a-set) 2) (cons a-set nil)
    (let [rots (rotations a-set)
          f (fn [f-seq] (prefix (first f-seq) (permutations (rest f-seq))))]
      (apply concat (map f rots)))))

(defn subsets [a-seq a-set]
  (if (empty? a-seq) #{}
    (conj (subsets (rest a-seq) a-set) (disj a-set (first a-seq)))))

(defn powerset [x-set]
  (let [a-set (set x-set)]
  (if (empty? a-set) #{#{}}
    (let [s (seq a-set)]
      (clojure.set/union #{a-set} (apply clojure.set/union (map powerset (subsets (seq a-set) a-set))))))))

