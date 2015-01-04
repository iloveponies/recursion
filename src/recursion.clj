(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll)     nil
    (singleton? coll) (first coll)
    :else             (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq)     nil
    (singleton? a-seq) (first a-seq)
    :else              (max (first a-seq)
                            (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [len-1 (count seq-1)
        len-2 (count seq-2)]
    (if (> len-1 len-2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq)     nil
    (singleton? a-seq) (first a-seq)
    :else              (seq-max (first a-seq)
                                (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [front (first a-seq)
          back  (rest a-seq)]
      (if (pred? front)
        (cons front
              (my-filter pred? back))
        (my-filter pred? back)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)   false
    (= (first a-seq)
       elem)         true
    :else            (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [front (first a-seq)
          back  (rest a-seq)]
      (if (pred? front)
        (cons front
              (my-take-while pred? back))
        '()))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq)
         (empty? b-seq)) true
    (empty? a-seq)       false
    (empty? b-seq)       false
    (= (first a-seq)
       (first b-seq))    (seq= (rest a-seq)
                               (rest b-seq))
    :else                false))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) '()
    (empty? seq-2) '()
    :else          (cons (f (first seq-1)
                            (first seq-2))
                         (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n
       (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (let [less (dec up-to)]
      (cons less
            (my-range less)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (apply list a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat
               (tails a-seq)
               (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [top (first a-seq)]
      (my-frequencies-helper (assoc freqs top
                                    (if (contains? freqs top)
                                      (inc (freqs top))
                                      1))
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)
          back  (rest a-map)]
      (concat (repeat v k)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll)
          (zero? n))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll)
          (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq)
     (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)    b-seq
    (empty? b-seq)    a-seq
    (> (first a-seq)
       (first b-seq)) (cons (first b-seq)
                            (seq-merge a-seq (rest b-seq)))
    :else             (cons (first a-seq)
                            (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq)     a-seq
    (singleton? a-seq) a-seq
    :else              (let [[a b] (halve a-seq)]
                         (seq-merge (merge-sort a)
                                    (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [prefixes    (rest (inits a-seq))
          up          (my-take-while (fn [x] (apply <= x)) prefixes)
          down        (my-take-while (fn [x] (apply >= x)) prefixes)
          up-length   (count up)
          down-length (count down)]
      (if (> up-length down-length)
        (concat (drop (dec up-length) up)
              (split-into-monotonics (drop up-length a-seq)))
        (concat (drop (dec down-length) down)
              (split-into-monotonics (drop down-length a-seq)))))))

(defn permutations-helper [a-set]
  (if (empty? a-set)
    '(())
    (apply concat (map (fn [x]
                           (map (fn [y]
                                    (concat (list x) y))
                                (permutations-helper (disj a-set x))))
                       a-set))))

(defn permutations [a-set]
  (permutations-helper (set a-set)))

(defn powerset-helper [a-set]
  (if (empty? a-set)
    #{#{}}
    (set (map set
              (apply concat (map (fn [x]
                                     (let [pset (powerset-helper
                                                  (disj a-set x))]
                                       (concat (map (fn [y] (concat (list x) y))
                                                    pset)
                                               pset)))
                                 a-set))))))

(defn powerset [a-set]
  (powerset-helper (set a-set)))
