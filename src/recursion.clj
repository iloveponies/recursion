(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
  (cond 
   (empty? seq-1) seq-2
   (empty? seq-2) seq-1
   :else (let [r1 (rest seq-1)
               r2 (rest seq-2)
               s (seq-max r1 r2)]
           (if (and (empty? r1) (= s r1))  ; :(
             seq-1 seq-2))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)
          r (rest a-seq)]
       (if (pred? f) 
              (cons f (my-filter pred? r))
              (my-filter pred? r)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not (= elem (first a-seq))) (sequence-contains? elem (rest a-seq))
   :else true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons
                          (first a-seq)
                          (my-take-while pred? (rest a-seq)))
   :else '()
   ))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq
   ))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (let [f1 (first seq-1)
        f2 (first seq-2)
        r1 (rest seq-1)
        r2 (rest seq-2)]
    (if (or (empty? seq-1) (empty? seq-2)) 
      '()
      (cons (f f1 f2) (my-map f r1 r2)))))

(defn power [n k]
  (cond 
   (= k 0) 1
   (= 0 (mod k 2)) (* (power n (/ k 2)) (power n (/ k 2)))
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1))(fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat 
          (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [n a-seq]
  (let [rotato (concat (rest a-seq) (cons (first a-seq) '()))]
    (if (= n 0)
      '()
      (cons rotato (rotations-helper (- n 1) rotato)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
   (cons '() '())
   (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [item (first a-seq)
        re (rest a-seq)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (assoc 
                               freqs 
                               item 
                               (inc (or (get freqs item) 0)))
                             re))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [[wat nbr] (first a-map)]
    (if (empty? a-map)
      '()
      (concat (repeat nbr wat) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (>= 0 n) (empty? coll))
    '()
    (cons (first coll) 
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (>= 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [pivot (int (/ (count a-seq) 2))]
    (vec (cons (my-take pivot a-seq) (cons (my-drop pivot a-seq) '())))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)
        ra (rest a-seq)
        rb (rest b-seq)]
    (cond
     (and (empty? a-seq) (empty? b-seq)) '()
     (empty? a-seq) (cons b (seq-merge a-seq rb))
     (empty? b-seq) (cons a (seq-merge ra b-seq))
      :else (if (< a b)
              (cons a (seq-merge ra b-seq))
              (cons b (seq-merge a-seq rb))))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (let [[a b] (halve a-seq)
          sa (merge-sort a)
          sb (merge-sort b)]
      (seq-merge sa sb))))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [s] 
                     (or (apply <= s) (apply >= s)))
        longest-prefix (first (drop-while 
                               (complement monotonic?)
                               (inits a-seq)))] ;inits returns longest first
    (if (>= 2 (count a-seq))
      (cons a-seq '())
      (cons longest-prefix 
            (split-into-monotonics
             (drop
              (count longest-prefix)
              a-seq))))))

(defn permutations [a-set]
   (cond
    (> 2 (count a-set)) (cons a-set '())
    (= 2 (count a-set)) (rotations a-set)
    :else (let [pe (fn [x]
                    (map (fn [s]
                          (cons (first x) s))
                         (permutations (rest x))))]
           (apply concat (map pe (rotations a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [as (set a-set)
          sub (fn [s] (disj as s))]
       (clojure.set/union #{as} (apply clojure.set/union (map powerset (map sub as)))))))