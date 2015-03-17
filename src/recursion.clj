(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (and (seq coll) (empty? (rest coll)))
    true
    false))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))


(defn custom-max-element [maxfn a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (maxfn (first a-seq) (custom-max-element maxfn (rest a-seq))))))

(defn max-element [a-seq]
  (custom-max-element max a-seq))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) 
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (custom-max-element seq-max a-seq))

(defn my-filter [pred? a-seq]
  (cond 
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond 
    (= elem (first a-seq)) true
    (> (count a-seq) 1) (sequence-contains? elem (rest a-seq))
    :else false))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) 
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq))))))


(defn my-drop-while [pred? a-seq]
  (cond 
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond 
    (not (= (count a-seq) (count b-seq))) false
    (not (= (first a-seq) (first b-seq))) false
    (<= (count a-seq) (count b-seq) 1) true
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond 
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) 
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map (fn[n] (concat (drop n a-seq) (take n a-seq))) indexes)))  


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) 
    freqs
    (let [n (freqs (first a-seq))
          m (assoc freqs (first a-seq) (if (= nil n) 1 (inc n)))]
      (my-frequencies-helper m (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    nil
    (let [entry (first a-map)]
      (concat (my-repeat (entry 1) (entry 0)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (or (>= 0 n) (empty? coll)) '()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond 
    (>= 0 n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [k (int (/ (count a-seq) 2))]
    (list (my-take k a-seq) (my-drop k a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) a-seq
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a (first a-seq)
                b (first b-seq)]
            (if (<= a b)
              (cons a (seq-merge (rest a-seq) b-seq))
              (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn monotonic?[a-seq] (if (empty? a-seq) false (or (apply >= a-seq) (apply <= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) 
    a-seq
    (let [x (last (filter monotonic? (inits a-seq)))]
      (cons x (split-into-monotonics (drop (count x) a-seq))))))

(defn splice[f perm]
  (defn splice-help[f perm i]
    (if (> i (count perm))
      nil
      (concat (list (concat (take i perm) (cons f (drop i perm)))) (splice-help f perm (inc i))) ))
  (splice-help f perm 0))

(defn intersplice[f perms]
  (if (empty? perms) nil
  (concat (splice f (first perms)) (intersplice f (rest perms)))))


(defn permutations [a-set]
  (loop [f (first a-set)
         the-set (rest a-set)
         perms [[]]]
    (if (= nil f) perms
      (recur (first the-set) (rest the-set) (intersplice f perms)))))
(permutations #{1 2 3})
;;figure out how to do one-sets


(defn powerset [a-set])




