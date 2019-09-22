(ns recursion)

(defn product [coll]
  (if (empty? coll) 
    1
    (* (first coll) 
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll))
       (not (empty? coll))))

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
  (cond 
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond 
    (empty? a-seq) ()
    (pred? (first a-seq)) 
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (count a-seq) (count b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if 
    (or (empty? seq-1) (empty? seq-2)) ()
    (cons 
      (f (first seq-1) (first seq-2)) 
      (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond 
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons
      what-to-repeat
      (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons
      (dec up-to)
      (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons
      (seq a-seq)
      (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest 
      (my-map concat 
        (tails a-seq)
        (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          freq-count (freqs elem)
          new-freq-count (if (nil? freq-count) 
                           1
                           (inc freq-count))]
      (my-frequencies-helper (assoc freqs elem new-freq-count) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (second (first a-map)) 
                    (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons 
      (first coll) 
      (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) 
     (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [first-a (first a-seq)
        first-b (first b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (<= first-a first-b) (cons 
                             first-a 
                             (seq-merge (rest a-seq) b-seq))
      :else (cons 
              first-b
              (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond 
    (empty? a-seq) ()
    (== (count a-seq) 1) a-seq
    :else
      (let [[first-seq sec-seq] (halve a-seq)]
        (seq-merge 
          (merge-sort first-seq)
          (merge-sort sec-seq)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (let [monotonic (last (take-while monotonic? (rest (inits a-seq))))]
    (if (empty? a-seq)
    '()
    (cons 
      monotonic
      (split-into-monotonics (drop (count monotonic) a-seq))))))

(defn perm-helper [a-set acc]
  (if (empty? a-set)
    (list acc)
    (apply concat (map #(perm-helper 
                         (remove (fn [x] (= x %)) a-set) 
                         (cons % acc)) 
                       a-set))))

(defn permutations [a-set]
  (perm-helper a-set '()))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (clojure.set/union 
     (powerset (rest a-set))
     (map #(conj % (first a-set)) (powerset (rest a-set))))))
