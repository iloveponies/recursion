(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (= 1 (count coll)))

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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else (empty a-seq)))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (let [one-not-empty? (fn [a b]
                         (or (and (empty? a) (not (empty? b)))
                             (and (not (empty? a)) (empty? b))))
        both-empty? (fn [a b] (and (empty? a) (empty? b)))]
    (cond
     (both-empty? a-seq b-seq) true
     (one-not-empty? a-seq b-seq) false
     (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
     :else false)))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) (empty seq-2)
   :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (let [n how-many-times]
    (cond
     (<= n 0) '()
     :else (cons what-to-repeat (my-repeat (- n 1) what-to-repeat)))))

(defn my-range [up-to]
  (if (== up-to 0)
   '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (let [nester (fn [x] (seq [x]))]
    (if (empty? a-seq)
      (nester '())
      (cons (seq a-seq) (tails (rest a-seq))))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [seq-length (count a-seq)
        nester (fn [x] (seq [(seq x)]))
        next-rot (fn [some-seq] (concat (rest some-seq) (vector (first some-seq))))
        rotator (fn rot [accum b-seq]
                  (if (= b-seq a-seq)
                    accum
                    (rot (cons b-seq accum) (next-rot b-seq))))]
    (if (empty? a-seq)
      (list (empty '()))
      (rotator (nester a-seq) (next-rot a-seq)))))



(defn my-frequencies-helper [freqs a-seq]
  (let [remove-elems (fn [some-seq el]
                       (filter (fn [x] (not (= x el))) some-seq))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper
       (assoc
         freqs
         (first a-seq)
         (count (filter (fn [x] (= x (first a-seq))) a-seq)))
       (remove-elems a-seq (first a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper (empty {}) a-seq))

(defn un-frequencies [a-map]
  (let [my-little-helper (fn this [acc some-seq]
                           (if (empty? some-seq)
                             acc
                             (this
                              (concat acc (repeat
                                           (second (first some-seq))
                                           (first (first some-seq))))
                              (rest some-seq))))]
    (my-little-helper (empty {}) a-map)))

(defn my-take [n coll]
  (let [little-helper (fn lh [acc numb a-seq]
                        (if (or (empty? a-seq) (= 0 numb))
                          acc
                          (lh (cons (first a-seq) acc) (dec numb) (rest a-seq))))]
    (reverse (little-helper '() n coll))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-index (int (/ (count a-seq) 2))]
    [(my-take half-index a-seq) (my-drop half-index a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [little-helper (fn lh [acc seq-1 seq-2]
                        (cond
                         (empty? seq-1) (concat acc seq-2)
                         (empty? seq-2) (concat acc seq-1)
                         (< (first seq-1) (first seq-2)) (lh
                                                          (concat acc (list (first seq-1)))
                                                          (rest seq-1)
                                                          seq-2)
                         :else (lh
                                (concat acc (list (first seq-2)))
                                seq-1
                                (rest seq-2))))]
    (little-helper '() a-seq b-seq)))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (= 1 (count a-seq)))
    a-seq
    (let [halves (halve a-seq)]
      (seq-merge (merge-sort (first halves)) (merge-sort (second halves))))))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [some-seq]
                     (or (apply <= some-seq) (apply >= some-seq)))
        first-monotonic (fn [some-seq]
                          (first (drop-while
                                  (complement monotonic?)
                                  (filter (complement empty?) (inits some-seq)))))
        little-helper (fn lh [acc some-seq]
                        (if (empty? some-seq)
                          acc
                          (let [mon-seq (first-monotonic some-seq)
                                all-the-rest (drop (count mon-seq) some-seq)]
                            (lh (concat acc (list mon-seq)) all-the-rest))))]
    (little-helper '() a-seq)))

(defn permutations [a-set]
  (let [remove-at (fn [index]
                    (concat
                     (take index a-set)
                     (drop (+ 1 index) a-set)))
        add-heads (fn [head] (fn [a-seq] (map #(conj %1 head) a-seq)))
        demonic-f (fn [ind] ((add-heads (nth a-set ind)) (permutations (remove-at ind))))]
    (cond
     (empty? a-set) (list '())
     (= 1 (count a-set)) (list (seq a-set))
     :else (apply concat (map demonic-f (range 0 (count a-set)))))))


(defn powerset [a-seq]
  (let [a-set (set a-seq)
        union clojure.set/union]
    (cond
     (= 0 a-set) #{a-set}
     (= 1 a-set) #{a-set}
     :else (union #{#{}} #{a-set} (apply union (map #(powerset (disj a-set %1)) a-set))))))
