(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (boolean (and (not (empty? coll))
                (empty? (rest coll)))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq)
              (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq)
                  (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-take-while pred? (rest a-seq)))
   :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))


(defn seq= [a-seq b-seq]
  (if (empty? a-seq)
    (empty? b-seq)
    (and (not (empty? b-seq))
         (= (first a-seq)
            (first b-seq))
         (seq= (rest a-seq)
               (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    ()
    (cons (f (first seq-1)
             (first seq-2))
          (my-map f
                  (rest seq-1)
                  (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n
       (power n
              (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (defn helper [this-seq times]
    (if (zero? times)
      '()
      (cons this-seq
            (helper (concat (rest this-seq)
                            [(first this-seq)])
                    (dec times)))))
  (if (empty? a-seq)
    '(())
    (helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
      (let [cur-element (first a-seq)
            first-count (or (freqs cur-element)
                            0)]
        (my-frequencies-helper (assoc freqs cur-element (inc first-count))
                               (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[element times] (first a-map)]
      (concat (my-repeat times element)
              (un-frequencies (dissoc a-map element))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n)
                   (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n)
             (rest coll))))

(defn halve [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))]
    (vector (my-take midpoint a-seq)
            (my-drop midpoint a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        :else (let [first-a (first a-seq)
                    first-b (first b-seq)]
                (if (< first-a first-b)
                  (cons first-a
                        (seq-merge (rest a-seq)
                                   b-seq))
                  (cons first-b
                        (seq-merge a-seq
                                   (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half)
                 (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [initial (last (take-while (fn [beginning-of-list]
                                      (or (apply < beginning-of-list)
                                          (apply > beginning-of-list)))
                                    (rest (inits a-seq))))]
      (cons initial
            (split-into-monotonics (my-drop (count initial)
                                            a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (apply concat
           (map (fn [element]
                  (map (fn [result-sequence]
                         (concat [element]
                                 result-sequence))
                       (permutations (remove (fn [check-ele] (= check-ele element))
                                             a-set))))
                a-set))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [cur (first a-set)
          powerset-without-first (powerset (rest a-set))]
      (concat (set (map (fn [other-set]
                          (conj other-set cur))
                        powerset-without-first))
              powerset-without-first))))
