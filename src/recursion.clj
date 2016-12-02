








(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))


(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))



(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))



(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or (= elem (first a-seq))
        (sequence-contains? elem (rest a-seq)))))



(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
      ())))




(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))


(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
     false
     (and (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq))))))

(seq= [nil] [])

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))



(defn power [n k]
  (if (== k 0)
    1
    (if (= n 1)
      n
      (* n (power n (dec k))))))


(defn fib [n]
  (if (= n 1)
    1
    (if (= n 0)
      0
    (+ (fib (- n 1)) (fib (- n 2))))))



(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    ()
    (cons what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))))



(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (reverse (into () a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))


(defn rotations [a-seq]
  (distinct (map concat (tails a-seq) (inits a-seq))))



(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (contains? freqs (first a-seq))
      (my-frequencies-helper
       (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
      (my-frequencies-helper
       (assoc freqs (first a-seq) 1) (rest a-seq)))))


(defn my-frequencies [a-seq]
    (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (second (first a-map)) (first (first a-map)))
            (un-frequencies (rest a-map)))))



(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    ()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(my-take 1 (list 1 2 3))


(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(my-drop 0 (list 1 2 3))

(defn halve [a-seq]
  (vector (my-take (int (/ (count a-seq) 2)) a-seq)
          (my-drop (int (/ (count a-seq) 2)) a-seq)))

(first (halve (list 1)))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
    a-seq
  (if (<= (first a-seq) (first b-seq))
    (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
  (if (> (count a-seq) 1)
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))
    a-seq))

  (merge-sort (list 9 8 7 6 5 4 3 2 1))

  (defn increasing? [a-seq]
  (if (= 1 (count a-seq))
    true
    (and (< (first a-seq) (second a-seq)) (increasing? (rest a-seq)))))

(defn decreasing? [a-seq]
  (if (= 1 (count a-seq))
    true
    (and (> (first a-seq) (second a-seq)) (decreasing? (rest a-seq)))))


  (defn monotonic? [a-seq]
  (or (increasing? a-seq) (decreasing? a-seq)))



(defn split-into-monotonics [a-seq]
  (if (monotonic? a-seq)
    (list a-seq)
    (let [x (last (filter monotonic? (rest (inits a-seq))))]
      (cons x
        (split-into-monotonics (my-drop (count x) a-seq))))))






(defn permutations [a-set]
  (let [a (set a-set)]
    (if (>= 1 (count a))
      (list (into () a))
      (apply concat
             (map (fn [x]
                  (map (fn [y]
                         (cons x y))
                         (permutations (disj a x))))
                   a)))))

(count (permutations (range 5)))


(clojure.set/union #{1 2} #{1 2})

(defn powerset [a-set]
 (if (empty? a-set) #{#{}}
      (let [x (powerset (rest a-set))]
        (clojure.set/union x
                           (map #(conj % (first a-set)) (powerset (next a-set)))))))

(powerset #{1 2 3})
(next #{1})

