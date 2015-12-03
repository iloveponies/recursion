(ns recursion)

(defn product [coll]
  (cond (empty? coll) 1
        :else
        (* (first coll)
           (product (rest coll)))))

(defn singleton? [coll]
  (cond (empty? coll) false
        (empty? (rest coll)) true
        :else false))

(defn my-last [coll]
  (cond (empty? coll) nil
        (singleton? coll) (first coll)
        :else
        (recur (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else
        (max (first a-seq)
             (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else
        (seq-max (first a-seq)
                 (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        :else
        (if (pred? (first a-seq))
          (cons (first a-seq)
                (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else
        (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) ()
        (not (pred? (first a-seq))) ()
        :else
        (cons (first a-seq)
              (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (not (pred? (first a-seq))) a-seq
        :else
        (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq)
        (empty? b-seq)
        (not= (first a-seq) (first b-seq))) false
    :else
    (recur (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) seq-1
    (empty? seq-2) seq-2
    :else
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
        (+ (fib (- n 1))
           (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond (<= how-many-times 0) ()
        :else
        (cons what-to-repeat
              (my-repeat
                (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond (<= up-to 0) ()
        :else
        (cons (dec up-to)
              (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond (empty? a-seq) '(())
        :else
        (cons (seq a-seq)
              (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [reverse-seq (reverse a-seq)]
    (map reverse
         (tails reverse-seq))))

(defn rotations [a-seq]
  (cond (empty? a-seq) '(())
        :else
        (let [tail-seq (tails a-seq)
              init-seq (reverse (rest (inits a-seq)))]
          (map concat tail-seq init-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else
    (let [new-freqs
          (if (contains? freqs (first a-seq))
            (assoc freqs (first a-seq)
                         (inc (freqs (first a-seq))))
            (assoc freqs (first a-seq) 1))]
      (my-frequencies-helper
        new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond (empty? a-map) ()
        :else
        (let [[fst snd] (first a-map)]
          (concat (repeat snd fst)
                  (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond (empty? coll) ()
        (<= n 0) ()
        :else
        (cons (first coll)
              (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (empty? coll) ()
        (<= n 0) coll
        :else
        (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [size (count a-seq)
        half (int (/ size 2))]
    (vector (my-take half a-seq)
            (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        :else
        (if (<= (first a-seq) (first b-seq))
          (cons (first a-seq)
                (seq-merge (rest a-seq) b-seq))
          (cons (first b-seq)
                (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq) a-seq
        (singleton? a-seq) a-seq
        :else
        (let [[fst snd] (halve a-seq)]
          (seq-merge (merge-sort fst)
                     (merge-sort snd)))))

(defn mono-helper [pred? a-seq]
  (cond (empty? a-seq) true
        (singleton? a-seq) true
        :esle
        (and (pred? (first a-seq)
                    (first (rest a-seq)))
             (recur pred? (rest a-seq)))))

(defn mono-inc? [a-seq]
  (mono-helper <= a-seq))

(defn mono-dec? [a-seq]
  (mono-helper >= a-seq))

(defn monotonic? [a-seq]
  (or (mono-inc? a-seq) (mono-dec? a-seq)))

(defn split-into-monotonics [a-seq]
  (cond (empty? a-seq) a-seq
        (singleton? a-seq) a-seq
        :else
        (let [mono-seq (first
                         (reverse
                           (take-while monotonic?
                                       (reverse (inits a-seq)))))]
          (cons mono-seq
                (split-into-monotonics (drop (count mono-seq) a-seq))
                ))))

(defn permutations [a-set]
  "TODO"
  (cond (empty? a-set) '(())
        (singleton? a-set) a-set
        :else
        a-set))

(defn powerset [a-set]
  "TODO"
  (cond (empty? a-set) #{#{}}
        :else
        a-set))

