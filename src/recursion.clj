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

(defn _max-element [a-seq current-max]
  (if (empty? a-seq)
    current-max
    (_max-element (rest a-seq)
                  (max current-max
                       (first a-seq)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (_max-element (rest a-seq)
                  (first a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn _longest-sequence [a-seq current-longuest-seq]
  (if (empty? a-seq)
    current-longuest-seq
    (_longest-sequence (rest a-seq)
                       (seq-max current-longuest-seq
                                (first a-seq)))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (_longest-sequence (rest a-seq)
                       (first a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    (list)
    (let [item   (first a-seq)
          values (my-filter pred? (rest a-seq))]
      (if (pred? item)
        (cons item values)
        values))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= elem (first a-seq)) true
    :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        ()
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else                 ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)        ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))       true
    (and (not (empty? a-seq)) (empty? b-seq)) false
    (and (empty? a-seq) (not (empty? b-seq))) false
    (= (first a-seq) (first b-seq))           (seq= (rest a-seq) (rest b-seq))
    :else                                     false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= n 1)   1
   :else     (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list (vector))
    (cons (apply vector a-seq)
          (tails (rest a-seq)))))

(defn first-n [seq n]
  (if (or (empty? seq) (= n 0))
    ()
    (cons (first seq) (first-n (rest seq) (dec n)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list (vector))
    (cons (apply vector a-seq)
          (inits (first-n a-seq (dec (count a-seq)))))))

(defn last-n [seq n]
  (reverse (first-n (reverse seq) n)))

(defn rotate [a-seq n]
  (apply vector (concat (last-n a-seq n)
                        (first-n a-seq (- (count a-seq) n)))))

(defn _rotations [a-seq n]
  (if (zero? n)
    (list a-seq)
    (cons (rotate a-seq n) (_rotations a-seq (dec n)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list (vector))
    (_rotations a-seq (dec (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
          freq (get freqs item)]
      (my-frequencies-helper
        (if (nil? freq)
          (assoc freqs item 1)
          (assoc freqs item (inc freq)))
         (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[key num] (first a-map)]
      (concat (repeat num key)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (first-n coll n))

(defn my-drop [n coll]
  (if (> n (count coll))
    ()
    (last-n coll n)))

(defn halve [a-seq]
  (let [len (count a-seq)
        n   (int (/ len 2))]
    [(my-take n a-seq)
     (my-drop (- len n) a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))       ()
    (and (not (empty? a-seq)) (empty? b-seq)) a-seq
    (and (empty? a-seq) (not (empty? b-seq))) b-seq
    (> (first a-seq) (first b-seq))           (concat (list (first b-seq))
                                                      (seq-merge a-seq (rest b-seq)))
    :else                                     (concat (list (first a-seq))
                                                      (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  (let [len (count a-seq)]
    (if (or (= len 1) (= len 0))
      a-seq
      (let [[first-part second-part] (halve a-seq)]
        (seq-merge (merge-sort first-part)
                   (merge-sort second-part))))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    false
    (or (apply <= a-seq)
        (apply >= a-seq))))

(defn calculate-monotonic [a-seq]
  (last (filter monotonic? a-seq)))

;([] [1] [1 2] [1 2 6] [1 2 6 7] [1 2 6 7 2] [1 2 6 7 2 3])
;[1 2 6 7]

(defn calculate-new-seq [a-seq sub-seq]
  (let [len     (count sub-seq)
        new-seq (filter (fn [seq1] (> (count seq1) len)) a-seq)]
    (map (fn [coll] (drop len coll)) new-seq)))

(defn split-into-monotonics-helper [a-seq]
  (if (empty? a-seq)
    ()
    (let [monotonic (calculate-monotonic a-seq)
          new-seq   (calculate-new-seq a-seq monotonic)]
      (cons monotonic (split-into-monotonics-helper new-seq)))))


(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper (reverse (inits a-seq))))


(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

