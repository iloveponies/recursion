(ns recursion)

(defn product [coll]
  (if (empty? coll)
      1
      (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (cond (empty? coll) false
        (empty? (rest coll)) true
        :else false))

(defn my-last [coll]
  (if (empty? (rest coll))
      (first coll)
      (my-last (rest coll))))

(defn max-element [a-seq]
    (if (empty? a-seq)
        nil
        (if (singleton? a-seq)
            (first a-seq)
            (max (first a-seq) (max-element (rest a-seq))))))

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
    a-seq
    (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
     (empty? a-seq)
       false
     (== elem (first a-seq))
       true
     :else
       (sequence-contains? elem (rest a-seq))))


(defn my-take-while [pred? a-seq]
  (cond
     (empty? a-seq)
       '()
     (pred? (first a-seq))
       (conj (my-take-while pred? (rest a-seq)) (first a-seq))
     :else
       '()))

(defn my-drop-while [pred? a-seq]
    (cond
     (empty? a-seq)
       '()
     (pred? (first a-seq))
       (my-drop-while pred? (rest a-seq))
     :else
       (apply list a-seq)))

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
  (cond
     (and (empty? seq-1) (empty? seq-2))
       '()
     (or (empty? seq-1) (empty? seq-2))
       '()
     :else
       (conj (my-map f (rest seq-1) (rest seq-2))
             (f (first seq-1) (first seq-2)))))

(defn power [n k]
  (if
    (zero? k)
      1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n)
      0
    (= 1 n)
      1
    :else
     (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat)))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (conj (tails (rest a-seq)) (apply list a-seq))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations-helper [n a-seq]
  (if (>= 0 n)
    (list a-seq)
    (conj (rotations-helper (dec n) (rest a-seq))
          (take (- (count a-seq) n) a-seq))))

(defn rotations [a-seq]
  (rotations-helper (dec (count a-seq)) (concat (rest a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [a-key (first a-seq)
          prev-freq (freqs a-key)
          new-freq (if (nil? prev-freq)
                 1
                 (inc prev-freq))]
      (my-frequencies-helper (assoc freqs a-key new-freq) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [a-key (key (first a-map))
          a-value (val (first a-map))]
      (concat (repeat a-value a-key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (conj (my-take (dec n) (rest coll)) (first coll))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
     (empty? a-seq) (apply list b-seq)
     (empty? b-seq) (apply list a-seq)
     :else (let [a (first a-seq)
                 b (first b-seq)]
             (if (< a b)
               (conj (seq-merge (rest a-seq) b-seq) a)
               (conj (seq-merge a-seq (rest b-seq)) b)))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq)
          '()
        (singleton? a-seq)
          a-seq
        :else
          (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
      (list (apply list a-seq))
    :else
      (let [the-inits (inits a-seq)
            max-monotonic (fn [f] (last (take-while #(apply f %) (rest (inits a-seq)))))
            up (max-monotonic <)
            down (max-monotonic >)
            monotonic (if (> (count up) (count down)) up down)]
      (conj (split-into-monotonics (drop (count monotonic) a-seq)) monotonic))))

(defn permutations [a-set]
  (cond
     (empty? a-set)
       '(())
     (singleton? a-set)
       (list (apply list a-set))
     :else
       (apply concat (map
          (fn [the-set]  (map #(conj % (first the-set))
                             (permutations (rest the-set))))
          (rotations a-set) ))))

(defn powerset [a-set]
  (cond
     (empty? a-set)
       #{#{}}
     :else
       (clojure.set/union
         (apply clojure.set/union
                (map #(powerset (rest %)) (rotations a-set)))
         #{(set a-set)})))

