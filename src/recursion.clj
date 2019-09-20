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
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))


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
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [first-seq (first a-seq)
          my-filter-rest (my-filter pred? (rest a-seq))]
      (if (pred? first-seq)
        (cons first-seq my-filter-rest)
        my-filter-rest))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    ()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
    (if (or (empty? a-seq)
            (not (pred? (first a-seq))))
      a-seq
      (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (not= (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
         (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

;(fib 100)



(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times) ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())

(defn inits [a-seq]
  (reverse (map  reverse (tails (reverse a-seq)))))

(defn my-rotations [a-seq n]
         (cond
          (<= n 0) (list a-seq)
          ;(= n 1) (List a-seq)
          :else (cons a-seq
                 (my-rotations
                  (concat (rest a-seq) [(first a-seq)])
                  (dec n)))))


(defn rotations [a-seq]
  (my-rotations a-seq (dec (count a-seq))))

(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
; The order of rotations does not matter.
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          freq (get freqs k 0)
          new-freqs (assoc freqs k (inc freq))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (if (empty? a-map)
    a-seq
    (let [[k v] (first a-map)
          seq-item (repeat v k)
          new-seq (concat a-seq seq-item)]
      (un-frequencies-helper new-seq (rest a-map)))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take-helper [acc n coll]
  (if (or (<= n 0) (empty? coll))
    acc
    (my-take-helper (cons (first coll) acc) (dec n)(rest coll))))

(defn my-take [n coll]
  (reverse (my-take-helper '() n coll))
  )

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge-helper [acc a-seq b-seq]
  (cond
   (empty? a-seq) (concat acc b-seq)
   (empty? b-seq) (concat acc a-seq)
   :else (let [fst-a (first a-seq)
               fst-b (first b-seq)]
           (if (< fst-a fst-b)
             (seq-merge-helper (concat acc (list fst-a)) (rest a-seq) b-seq)
             (seq-merge-helper (concat acc (list fst-b)) (rest b-seq) a-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (singleton? a-seq) a-seq
   :else (let [[fst sec] (halve a-seq)]
           (seq-merge (merge-sort fst) (merge-sort sec)))))

(defn monotonous? [a-seq]
  (or (apply = true (map < a-seq (rest a-seq)))
      (apply = true (map > a-seq (rest a-seq)))))


(defn split-into-monotonics-helper [acc a-seq]
    (if (empty? a-seq)
      (reverse acc)
      (let [ini (inits a-seq)
            mono-ini-seq (take-while #(monotonous? %) ini)
            mono-ini (last mono-ini-seq)]
        (split-into-monotonics-helper (cons mono-ini acc) (drop (count mono-ini) a-seq)))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper '() a-seq))

(defn insert-at-nth-pos [x n a-seq]
  (let [pre (take n a-seq)
        post (drop n a-seq)]
    (concat pre (cons x post))))

;(insert-at-nth-pos 1 0 '())

(defn insert-one-at-all-places [x a-seq]
  (map #(insert-at-nth-pos x % a-seq) (range (inc (count a-seq)))))

;(insert-one-at-all-places 3 [])

(defn permutations [a-set]
  (cond
   (empty? a-set) '(())
   ;(singleton? a-set) a-seq
   :else (let [fst (first a-set)
               perm-rst-set (permutations (rest a-set))
               ]
           (mapcat #(insert-one-at-all-places fst %) perm-rst-set))))


(defn powerset [a-seq]
  (if (empty? a-seq)
    #{#{}}
    (let [a-set (set a-seq)
          fst (first a-set)
          a-set-new (disj a-set fst)
          sub-powerset-wo-fst (powerset a-set-new)
          sub-powerset-w-fst (map #(conj % fst) sub-powerset-wo-fst)]
      (clojure.set/union sub-powerset-wo-fst sub-powerset-w-fst))))

