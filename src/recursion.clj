(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (not (empty? (rest coll))) false
    (empty? coll) false
    :else true))

(defn my-last [coll]
  (cond
    (singleton? coll) (first coll)
    (empty? coll) nil
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    :else (if (pred? (first a-seq))
            (cons (first a-seq) (my-filter pred? (rest a-seq)))
            (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (cons (first a-seq) (my-drop-while (fn [x] false) (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons
            (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= k 0) 1
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (> 1 how-many-times) '()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (> 1 up-to) '()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons (seq a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (let [tails (tails a-seq)
        inits (reverse (inits a-seq))]
    (rest (map concat tails inits))))

(defn my-frequencies-helper [freq-map a-seq]
  (if (empty? a-seq)
    freq-map
    (let [front (first a-seq)
          back (rest a-seq)
          new-freq-map (if (contains? freq-map front)
                         (assoc freq-map front (inc (get freq-map front)))
                         (assoc freq-map front 1))]
      (my-frequencies-helper new-freq-map back))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (let [new-n (dec n)]
    (if (or (empty?  coll) (< n 1))
      ()
      (cons (first coll) (my-take new-n (rest coll))))))

(defn my-drop [n coll]
  (let [new-n (dec n)]
    (if (empty? coll)
      ()
      (if (< n 1)
        (cons (first coll) (my-drop new-n (rest coll)))
        (my-drop new-n (rest coll))))))

(defn halve [a-seq]
  (let [front (int (/ (count a-seq) 2))]
    [(my-take front a-seq) (my-drop front a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq) '()
    (singleton? a-seq) a-seq
    :else (let [[front back] (halve a-seq)]
            (seq-merge (merge-sort front) (merge-sort back)))))

;Some helper functions for split-into-monotonics
(defn count-help [n last-val some-seq]
  (let [new-n (if (< (first some-seq) last-val)
                (inc n)
                n)]
    (cond
      (empty? some-seq) n
      (singleton? some-seq) (if (< (first some-seq) last-val)
                              (inc n)
                              n)
      :else (count-help new-n (first some-seq) (rest some-seq)))))

(defn count-monotonics [the-seq]
  (cond
    (empty? the-seq) 0
    (singleton? the-seq) 1
    :else (count-help 1 (first the-seq) (rest the-seq))))

(count-monotonics [1]) ;=> 1
(count-monotonics [])  ;=> 0
(count-monotonics [10 12 13 1 3 1]) ;=> 3
(count-monotonics [1 2 3 1]) ;=> 2
(count-monotonics [-12 20 1 -3 2]) ;=> 3

(defn get-nth-monotonic [n some-seq]
  )

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

