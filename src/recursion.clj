(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

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
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
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
    (not (pred? (first a-seq))) '()
    :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) '()
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (concat (list what-to-repeat)
            (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations-helper [n a-seq]
  (if (<= n 0)
    '()
    (cons a-seq (rotations-helper (dec n) (concat (rest a-seq) (list (first a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [n (get freqs (first a-seq))
          n-or-1 (if (nil? n) 0 n)
          upd-freqs (assoc freqs (first a-seq) (inc n-or-1))]
      (my-frequencies-helper upd-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (or (nil? a-map) (empty? a-map))
    '()
    (let [k (key (first a-map))]
      (concat (my-repeat (get a-map k) k) (un-frequencies (dissoc a-map k))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [half (int (/ (count a-seq) 2))]
      (list (my-take half a-seq) (my-drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    '()
    (if (empty? a-seq)
      b-seq
      (let [smaller-than (fn [x] (< x (first a-seq)))]
        (seq-merge (rest a-seq)
                   (concat (my-take-while smaller-than b-seq)
                           (cons (first a-seq)
                                 (my-drop-while smaller-than b-seq))))))))

(defn merge-sort [a-seq]
  (if (empty? a-seq)
    '()
    (if (singleton? a-seq)
      a-seq
      (let [h-seq (vec (halve a-seq))
            fh (get h-seq 0)
            sh (get h-seq 1)]
      (seq-merge (merge-sort fh) (merge-sort sh))))))

(defn monotonics-helper [n a-seq]
  (let [ve (vec a-seq)
        increasing (fn [x] (< (get ve (dec n)) (get ve n)))]
    (cons (my-take-while increasing a-seq)
          (monotonics-helper 1 (my-drop-while increasing a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    ;;(monotonics-helper 1 a-seq)))
    a-seq))

(defn permutations-rotate [n a-set]
  (map (fn [x] (concat (my-take n a-set) x)) (rotations (my-drop n a-set))))

(defn permutations-helper [n a-set]
  (if (<= n 0)
    '()
    (let [rotate (map (fn [x] (permutations-rotate x a-set)) (range 1 (dec (count a-set))))]
      (concat rotate
              (permutations-helper (dec n)
                                   (concat (rest a-set)
                                           (list (first a-set))))))))

(defn permutations-helper-original [n a-set]
  (if (<= n 0)
    '()
    (let [rotate (map (fn [x] (concat (list (first a-set))
                                      x)) (rotations (rest a-set)))]
      (concat rotate
              (permutations-helper (dec n)
                                   (concat (rest a-set)
                                           (list (first a-set))))))))

(defn permutations [a-set]
  (let [new-set [1 2 3 4]]
  (if (empty? a-set)
    (cons '() '())
    (permutations-helper (count new-set) new-set))))

(defn powerset-helper [n a-set]
  (if (<= n 0)
    (cons #{} #{})
    (let [powset (map (fn [x] (set (concat (list (first a-set)) x)))
                      (inits (rest a-set)))]
      (set (concat powset
                   (powerset-helper (dec n)
                                    (concat (rest a-set)
                                            (list (first a-set)))))))))

(defn powerset [a-set]
  (if (empty? a-set)
    (cons #{} #{})
    (powerset-helper (count a-set) a-set)))
    ;;(map (fn [x] (concat (list (first a-set)) x))
      ;;   (inits (rest a-set)))))

