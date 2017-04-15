(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and
   (empty? (rest coll))
   (not (empty? coll))))

(defn my-last [coll]
  (cond
   (empty? coll)
     nil
   (singleton? coll)
     (first coll)
   :else
     (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq)
     nil
   (singleton? a-seq)
     (first a-seq)
   :else
     (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (letfn [(my-count [a-seq]
            (if (empty? a-seq)
              0
              (+ (my-count (rest a-seq)) 1)))]
    (if (> (my-count seq-1) (my-count seq-2))
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [element (first a-seq)
        rest-my-filtered (lazy-seq (my-filter pred? (rest a-seq)))]
    (cond
     (empty? a-seq)
       '()
     (pred? element)
       (cons element rest-my-filtered)
     :else
       rest-my-filtered)))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [elem (first a-seq)]
    (if (or (empty? a-seq) (not (pred? elem)))
      '()
      (cons elem (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (let [elem (first a-seq)
        rest-my-drop-while (lazy-seq (my-drop-while pred? (rest a-seq)))]
    (cond
     (empty? a-seq)
       '()
     (pred? elem)
       rest-my-drop-while
     :else
       (seq a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (empty? a-seq) (empty? b-seq))
     false
   (= (first a-seq) (first b-seq))
     (seq= (rest a-seq) (rest b-seq))
   :else
     false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (letfn [(fib-seq [n-2 n-1]
           (cons n-2 (lazy-seq (fib-seq n-1 (+ n-2 n-1)))))]
    (nth (fib-seq 0 1) n)))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (letfn [(rotations-helper [a-seq b-seq]
             (if (empty? a-seq)
               '()
               (cons (concat a-seq b-seq)
                     (rotations-helper (rest a-seq) (concat b-seq (take 1 a-seq))))))]
    (if (empty? a-seq)
      '(())
      (rotations-helper a-seq '()))))

(defn my-frequencies [a-seq]
  (letfn [(my-frequencies-helper [freqs a-seq]
            (let [inc-helper (fnil inc 0)]
              (if (empty? a-seq)
                freqs
                (my-frequencies-helper (assoc freqs (first a-seq) (inc-helper (get freqs (first a-seq))))
                                       (rest a-seq)))))]
    (my-frequencies-helper {} a-seq)))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[what-to-repeat how-many-times] (first a-map)]
      (concat (my-repeat how-many-times what-to-repeat)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split-pos (int (/ (count a-seq) 2))]
    [(my-take split-pos a-seq)
     (my-drop split-pos a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond (empty? a-seq)
            b-seq
          (empty? b-seq)
            a-seq
          (< a b)
            (cons a (seq-merge (rest a-seq) b-seq))
          (>= a b)
            (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[first-half last-half] (halve a-seq)]
      (seq-merge (merge-sort first-half)
                 (merge-sort last-half)))))

(defn monotonic? [a-seq]
  (or (empty? a-seq)
      (apply <= a-seq)
      (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [longest-monotonic (last (my-take-while monotonic? (inits a-seq)))]
      (cons longest-monotonic
            (split-into-monotonics (drop (count longest-monotonic) a-seq))))))

(defn permutations-helper [a-set a-seq]
  (if (empty? a-set)
    a-seq
    (map (fn [elem] (permutations-helper (clojure.set/difference a-set [elem])
                                         (cons elem a-seq)))
         a-set)))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (partition (count a-set) (flatten (permutations-helper (set a-set) [])))))

(defn powerset [a-set]
  (let [the-set (set a-set)]
    (if (empty? the-set)
      #{#{}}
      (apply
        clojure.set/union #{the-set}
        (map (fn [elem]
             (powerset (clojure.set/difference the-set [elem])))
             the-set)))))

