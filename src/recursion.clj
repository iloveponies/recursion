(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))
  ;The better solution: (apply * coll))
;(product [1 2 4])
;(product (cons 1 (cons 2 (cons 4 ()'))))
;(* 1 (product (cons 2 (cons 4 ()'))))
;(* 1 (* 2 (product (cons 4 ()'))))
;(* 1 (* 2 (* 4 (product ()'))))
;(* 1 (* 2 (* 4 1)))
;(* 1 (* 2 4))
;(* 1 8)
; => 8

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (== elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else []))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (not (= (count a-seq) (count b-seq))) false
        (and (empty? a-seq) (empty? b-seq)) true
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) []
        :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (= 1 n)   1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> how-many-times 0)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (> up-to 0)
    (cons (dec up-to) (my-range (dec up-to)))
    '()))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] a-seq)
  (cons a-seq (tails (vec (rest a-seq))))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons [] a-seq)
    (cons (vec a-seq) (inits (drop-last a-seq)))))

(defn rotate-helper [a-seq times-to-rotate rotated]
  (let [rotated-left-by-one (fn [s] (take (count s) (next (cycle s))))]
  (if (> times-to-rotate 0)
    (rotate-helper (rotated-left-by-one a-seq) (dec times-to-rotate) (conj rotated (vec a-seq)))
    rotated)))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotate-helper a-seq (count a-seq) '())))

(defn my-frequencies-helper [freqs a-seq]
  (let [current (first a-seq)
        frequency-of (fn [k] (if (contains? freqs k) (get freqs k) 0))
        increased-frequency-of (fn [k] (assoc freqs k (inc (frequency-of k))))]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (increased-frequency-of current) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [what-to-repeat (first (first a-map))
        times-to-repeat (second (first a-map))]
  (if (empty? a-map)
    []
    (vec (concat (repeat times-to-repeat what-to-repeat) (un-frequencies (rest a-map)))))))

(defn my-take [n coll]
  (if (and (> n 0) (not (empty? coll)))
    (cons (first coll) (my-take (dec n) (rest coll)))
    '()))

(defn my-drop [n coll]
  (if (> n 0)
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (vector (my-take half a-seq) (my-drop half a-seq))))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond (empty? a-seq) b-seq
          (empty? b-seq) a-seq
          (< a b) (cons a (seq-merge (rest a-seq) b-seq))
          :else (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (last (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (let [reversed-inits (reverse (inits a-seq))
        monotonic? (fn [s] (if (empty? s)
                             false
                             (or (apply <= s) (apply >= s))))
        longest-monotonic (last (filter monotonic? reversed-inits))]
    (if (empty? a-seq)
      '()
    (cons longest-monotonic (split-into-monotonics (drop (count longest-monotonic) a-seq))))))

(defn find-k [a-set]
  (loop [index 0
         current-k nil
         a-seq a-set]
    (let [[current-element next-element] a-seq]
      (cond (nil? next-element) current-k
            (>= current-element next-element) (recur (inc index) current-k (rest a-seq))
            :else (recur (inc index) index (rest a-seq))))))

(defn find-l [k a-set]
  (loop [index 0
         l nil]
    (let [current-element (get a-set index)
          kth-element (get a-set k)]
      (cond (nil? current-element) l
            (<= kth-element current-element) (recur (inc index) index)
            :else (recur (inc index) l)))))

(defn permutation-of [a-set]
  (let [k (find-k a-set)
        l (find-l k a-set)
        swap (fn [s index1 index2]
               (assoc (vec s) index1 (get s index2) index2 (get s index1)))
        reverse-tail-from-index (fn [from-index s] (concat (take (+ from-index 1) s) (reverse (nthrest s (+ from-index 1)))))]
       (reverse-tail-from-index k (swap a-set k l))))

(defn permutations [a-set]
  (let [last-permutation? (fn [p] (nil? (find-k p)))
        initial-permutation (vec (sort a-set))]
  (loop [perms [initial-permutation]
         current-permutation initial-permutation]
      (if (last-permutation? current-permutation)
        perms
        (recur (conj perms (vec (permutation-of current-permutation))) (vec (permutation-of current-permutation)))))))

(defn powerset [a-set]
  [:-])


