(ns recursion)

(defn product [coll]
  "(reduce * 1 coll)"
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (loop [maksimi (first a-seq)
           xs      (rest a-seq)]
      (if (empty? xs)
        maksimi
        (recur (max maksimi (first xs)) (rest xs))))))

(defn seq-max [seq-2 seq-1]
  (if (> (count seq-2) (count seq-1))
    seq-2
    seq-1))

(defn longest-sequence [b-seq]
  (let [ln-seq (fn [a-seq] (if (empty? a-seq)
                             a-seq
                             (seq-max (first a-seq) (longest-sequence (rest a-seq)))))]
    (if (empty? b-seq)
      nil
      (ln-seq b-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (empty? a-seq)
    (if (empty? b-seq)
      true
      false)
    (if (empty? b-seq)
      false
      (and (= (first a-seq) (first b-seq))
           (seq= (rest a-seq) (rest b-seq))))))

(defn my-map [f seq-1 seq-2]
  (if (empty? seq-1)
    seq-1
    (if (empty? seq-2)
      seq-2
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (cond 
   (= k 0) 1
   (even? k) (* (power n (/ k 2)) (power n (/ k 2)))
   :else (* n (power n (dec k)))))

(defn fib [n]
  (loop [a 0
         b 1
         c n]
    (if (= c 0)
      a
      (recur b (+ a b) (dec c)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (conj (tails (rest a-seq))
          (apply list a-seq))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '()) ;; Testeissä on ihan kohtuuttomia vaatimuksia! :)
    (loop [n (count a-seq)
           result '()]
      (if (= n 0)
        result
        (recur (dec n) (cons (concat (drop n a-seq) (take n a-seq))
                             result))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [x (first a-seq)
          xs (rest a-seq)]
      (my-frequencies-helper (if (contains? freqs x)
                               (update-in freqs [x] inc)
                               (assoc freqs x 1))
                             xs))))

(defn my-frequencies [a-seq]
  "(group-by = a-seq)"
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply vector (flatten (map (fn [x] (my-repeat (second x) (first x))) a-map))))

(defn my-take [n coll]
  (if (empty? coll)
    coll
    (if (= n 0)
      nil
      (cons (first coll)
            (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if (empty? coll)
    coll
    (if (= n 0)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [len (count a-seq)
        half (if (odd? len)
               (/ (- len 1) 2)
               (/ len 2))]
    [(take half a-seq) (drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (let [a1 (first a-seq)
            b1 (first b-seq)]
        (if (< b1 a1)
          (cons b1
                (seq-merge a-seq (rest b-seq)))
          (cons a1
                (seq-merge (rest a-seq) b-seq)))))))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    '()
    (if (empty? (rest a-seq))
      a-seq
      (let [[a b] (halve a-seq)]
        (seq-merge (merge-sort a) (merge-sort b))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [ins (reverse (inits a-seq))
          monotonic? (fn [x] (or (< (count x) 3) (apply <= x) (apply >= x)))
          x (last (take-while monotonic? ins))
          rest (drop (count x) a-seq)]
      (cons x
            (split-into-monotonics rest)))))

(defn permutations 
  [b-set]
  "Ah, so beautiful! see more: (source permutations)"
  (let [perm (fn perm [a-set]
               (if (< (count a-set) 2)
                 (list a-set)
                 (map
                  (fn [x] (let [j (first x)
                                k (rest x)]
                            (map (partial cons j) (perm k))))
                  (rotations a-set))))]
    (if (empty? b-set)
      '(())
      (partition (count b-set) (flatten (perm b-set))))))

        
(defn powerset [a-set]
  (if (empty? a-set)
    (set [(set [])])
    (clojure.set/union (powerset (rest a-set))
                       (map (partial clojure.set/union #{(first a-set)})
                            (powerset (rest a-set))))))