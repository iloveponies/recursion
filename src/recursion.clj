(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
   (empty? coll) false
   (empty? (rest coll)) true
   :else false))

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
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1))
            (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
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
    (list a-seq)
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list a-seq)
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc
                             freqs
                             (first a-seq)
                             (if (contains? freqs (first a-seq))
                               (+ (get freqs (first a-seq)) 1)
                               1))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat
             (second (first a-map))
             (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
   (empty? coll) '()
   (= n 0) '()
   :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (empty? coll) '()
   (= n 0) coll
   :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [[head tail] (halve a-seq)]
    (if (<= (count a-seq) 1)
      a-seq
      (seq-merge (merge-sort head) (merge-sort tail)))))

(defn split-into-monotonics [a-seq]
  (let [mono (fn [ord]
               (last
                (take-while
                 (fn [a-list]
                   (if (< (count a-list) 2)
                     true
                     (ord (last a-list) (last (butlast a-list)))))
                 (sort-by count (inits a-seq)))))
        asc (mono >=)
        desc (mono <=)
        longest (if (>= (count asc) (count desc)) asc desc)]
     (if (empty? a-seq)
       '()
       (cons
        longest
        (split-into-monotonics (drop (count longest) a-seq))))))

(defn permutations [a-set]
  (let [permutations-aux
        (fn permutations-aux [a-list]
          (if (empty? a-list)
            '(())
            (apply
             concat
             (map
              (fn [s]
                (map
                 (fn [i]
                   (concat (take i s) (list (first a-list)) (drop i s)))
                 (range (inc (count s)))))
              (permutations-aux (rest a-list)))
             )))]
    (permutations-aux (into '() a-set))))

(defn powerset [a-set]
  (let [s (set a-set)]
    (if (empty? s)
      #{#{}}
      (clojure.set/union
       #{s}
       (set (apply clojure.set/union
                   (set (map powerset (map (fn [x] (disj s x)) s)))))))))
