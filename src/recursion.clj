(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and ((complement empty?) coll) (empty? (rest coll))))

(defn max-one? [coll]
  (or (empty? coll) (singleton? coll)))

(defn my-last [coll]
  (if (max-one? coll)
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (max-one? a-seq)
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn first-bigger? [s1 s2]
  (cond
   (empty? s1) false
   (empty? s2) true
   :else (first-bigger? (rest s1) (rest s2))))

(defn seq-max [seq-1 seq-2]
  (if (first-bigger? seq-1 seq-2) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (max-one? a-seq)
    (first a-seq)
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
  (let [[fst] a-seq]
    (cond
     (empty? a-seq) a-seq
     (pred? fst) (cons fst (my-take-while pred? (rest a-seq)))
     :else '())))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) ((complement pred?) (first a-seq)))
    a-seq
    (my-drop-while pred? (rest a-seq))))


(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (not (= (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn next-rotation [n a-seq]
  (if (< n 1)
    (cons a-seq '())
    (cons a-seq (next-rotation (dec n)
                               (concat (rest a-seq) (cons (first a-seq) '()))))))

(defn rotations [a-seq]
  (next-rotation (dec (count a-seq)) a-seq))

(defn my-frequencies-helper [freqs a-seq]
  (let [[frs] a-seq
        new-freqs (if (freqs frs)
                    (assoc freqs frs (inc (freqs frs)))
                    (assoc freqs frs 1))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [[ch times] (first a-map)]
    (if (empty? a-map)
      '()
      (concat (repeat times ch) (un-frequencies (rest a-map))))
    ))

(defn my-take [n coll]
  (let [[fst] coll]
    (cond
     (empty? coll) coll
     (> n 0) (cons fst (my-take (dec n) (rest coll)))
     :else '())))

(defn my-drop [n coll]
  (if (< n 1)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
     (vec (cons (take half a-seq) (cons (drop half a-seq) '() )))))

(defn seq-merge [a-seq b-seq]
  (let [[a1] a-seq
        [b1] b-seq
        both-empty? (and (not a1) (not b1))
        a-is-smaller? (and a1 (or (not b1) (< a1 b1)))]
    (cond
     both-empty? '()
     a-is-smaller? (cons a1 (seq-merge (rest a-seq) b-seq))
     :else (cons b1 (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [[s1 s2] (halve a-seq)
        sorted-1 (if (max-one? s1) s1 (merge-sort s1))
        sorted-2 (if (max-one? s2) s2 (merge-sort s2))]
    (seq-merge sorted-1 sorted-2)))

(defn monotonic? [a-seq]
  (or
   (max-one? a-seq)
   (apply > a-seq)
   (apply < a-seq)))

(defn split-into-monotonics [a-seq]
  (let [my-inits (inits a-seq)
        next-monotonic (last (take-while monotonic? my-inits))]
    (if (empty? a-seq)
      a-seq
      (cons next-monotonic (split-into-monotonics (drop (count next-monotonic) a-seq))))))

(defn permutations [a-set]
  (let [without (fn [x] (remove (fn [y] (= x y)) a-set))
        permutations-by-first-elem (fn [e] (map (fn [perm] (cons e perm)) (permutations (without e))))]
    (if (empty? a-set)
      (cons a-set '())
      (apply concat (map permutations-by-first-elem a-set)))))

(defn combine [f nodes]
  (if (empty? nodes)
    #{}
    (set (concat (f (first nodes)) (combine f (rest nodes))))))

(defn my-descendants [acc siblings]
  (let [children (fn [parent] (set (map (fn [elem] (disj parent elem)) parent)))
        next-gen (combine children siblings)
        new-acc (set (concat acc siblings))]
    (if (empty? siblings)
      (conj acc #{})
      (my-descendants new-acc next-gen))))

(defn powerset [a-set]
  (my-descendants #{} #{(set a-set)}))


;; (defn powerset [a-set]
;;   (let [s (set a-set)
;;         subsets (fn [elem] (powerset (disj s elem)))]
;;   (if (empty? s)
;;     #{s}
;;     (set (apply concat #{a-set} (map subsets s))))))

