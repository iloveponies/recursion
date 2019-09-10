(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(comment
  (my-last [])                          ;=> nil
  (my-last [1 2 3])                     ;=> 3
  (my-last [2 5])                       ;=> 5
  )

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max-element (cons (max (first a-seq) (second a-seq)) (drop 2 a-seq)))))

(comment
  (max-element [2 4 1 4])               ;=> 4
  (max-element [2])                     ;=> 2
  (max-element [])                      ;=> nil
  )

(defn seq-max [seq-1 seq-2]
  (cond 
    (> (count seq-1) (count seq-2)) seq-1
    (< (count seq-1) (count seq-2)) seq-2
    :else seq-2))


(defn longest-sequence [a-seq]
  (first (sort #(> (count %1) (count %2)) a-seq)))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)]
      (if (pred? f)
        (cons f (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? f) (cons f (my-take-while pred? (rest a-seq)))
     :else '())))

(comment
  (my-take-while odd? [1 2 3 4])        ;=> (1)
  (my-take-while odd? [1 3 4 5])        ;=> (1 3)
  (my-take-while even? [1 3 4 5])       ;=> ()
  (my-take-while odd? [])               ;=> ()
  )

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(comment
  (my-drop-while odd? [1 2 3 4])        ;=> (2 3 4)
  (my-drop-while odd? [1 3 4 5])        ;=> (4 5)
  (my-drop-while even? [1 3 4 5])       ;=> (1 3 4 5)
  (my-drop-while odd? [])               ;=> ()
  )

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(comment
  (seq= [1 2 4] '(1 2 4))               ;=> true
  (seq= [1 2 3] [1 2 3 4])              ;=> false
  (seq= [1 3 5] [])                     ;=> false
  )


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (< k 1) 1
   (= k 1) n
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (loop [coll a-seq
         acc '()]
    (if (empty? coll)
      (cons '() acc)
      (recur (butlast coll) (cons coll acc)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '())
    (loop [coll a-seq
           acc '()]
      (if (= (count coll) (count acc))
        acc
        (let [new (concat (rest coll) (vector (first coll)))]
          (recur new (cons new acc)))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper
     (let [f (first a-seq)]
       (if (contains? freqs f) (update-in freqs [f] inc) (assoc freqs f 1)))
     (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (flatten (map (fn [[k v]] (repeat v k)) a-map)))

(defn my-take [n coll]
  (loop [n (if (> n (count coll)) (count coll) n)
         coll coll
         acc []]
    (if (zero? n)
      acc
      (recur (dec n) (rest coll) (conj acc (first coll))))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (or (zero? n) (neg? n)) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split-at (int (/ (count a-seq) 2))]
    (vector (take split-at a-seq) (drop split-at a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [insert (fn [e coll]
                 (let [head (take-while #(< % e) coll)
                       tail (drop-while #(< % e) coll)]
                   (concat head [e] tail)))]
    (if (empty? a-seq)
      b-seq
      (seq-merge (rest a-seq)
                 (insert (first a-seq) b-seq)))))

(defn merge-sort [a-seq]
  (if (> (count a-seq) 1)
    (let [[head tail] (halve a-seq)]
      (seq-merge (merge-sort head) (merge-sort tail)))
    a-seq))

(defn split-at-repeat [a-seq]
  (loop [coll a-seq
         acc []]
    (if (or (empty? coll) (some #{(first coll)} acc))
      [acc coll]
      (recur (rest coll) (conj acc (first coll))))))

(defn split-into-monotonics [a-seq]
  (let [[a b] (split-at-repeat a-seq)]
    (if (empty? b)
      (partition 2 2 a)
      (concat [a] (split-into-monotonics b)))))

(defn permutations [a-set]
  (if (< (count a-set) 2)
    (list a-set)
    (for [head a-set
          tail (permutations (disj (set a-set) head))]
      (cons head tail))))

(defn powerset [a-set]
  (map set
       (if (empty? a-set) '(())
           (clojure.set/union
            (powerset (rest a-set))
            (map #(conj % (first a-set)) (powerset (rest a-set)))))))

