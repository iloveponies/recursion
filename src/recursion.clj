(ns recursion)

(defn product [coll]
  (apply * coll))


(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (if (= (count (rest a-seq)) 0)
     (max ((first a-seq) (first (rest a-seq))))
     (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))


(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (if (= (count (rest a-seq)) 0)
     (max ((first a-seq) (first (rest a-seq))))
     (max-element (rest a-seq)))))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))


(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
   false
   (if (= (first a-seq) elem)
     true
     (sequence-contains? elem (rest a-seq)))))


(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
   '()
   (if (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
     '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or
          (or (empty? a-seq) (empty? b-seq))
          (not (= (first a-seq) (first b-seq))))
      false
      (seq= (rest a-seq) (rest b-seq)))))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (+ (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (== n 0)
    0
    (if (== n 1)
      1
      (+ (fib (dec n)) (fib (dec (dec n)))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (reverse (into () a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))

 (defn my-frequencies-helper [freqs a-seq]
   (cond
     (empty? a-seq) freqs
     (contains?  freqs (first a-seq)) (let [freqs (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))] (my-frequencies-helper freqs (rest a-seq)))
     :else (let [freqs (assoc freqs (first a-seq) 1)] (my-frequencies-helper freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    ()))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [[el how-many] (first a-map)]
     (concat (repeat how-many el) (un-frequencies (rest a-map))))))


(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
   coll
   (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))


(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    b-seq
    (if (empty? b-seq)
      a-seq
      (if (> (first a-seq) (first b-seq))
        (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
        (cons (first a-seq) (seq-merge b-seq (rest a-seq)))))))



(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[f s] (halve a-seq)]
    (seq-merge (merge-sort f) (merge-sort s)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (= a-seq (sort a-seq)) (= a-seq (sort > a-seq)))))

(defn inits2 [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (monotonic? a-seq)
    [a-seq]
    (let [longest (first (reverse (take-while monotonic? (inits2 a-seq))))]
    (cons longest (split-into-monotonics (drop (count longest) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (let [permutations-helper (map (fn [x] (cons (first a-set) x)) (permutations (rest a-set)))]
      (apply concat (map rotations permutations-helper)))))


(defn powerset [a-set]
  (cond
    (empty? a-set) #{#{}}
    :else (clojure.set/union (powerset(next a-set)) (map #(conj % (first a-set)) (powerset (next a-set))))))


