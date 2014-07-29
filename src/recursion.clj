(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
      (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))


(defn my-last [coll]
  (cond (singleton? coll) (first coll)
        (empty? coll) nil
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (singleton? a-seq) (first a-seq)
        (empty? a-seq) nil
        :else (max (first a-seq)
                   (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
     seq-1
     seq-2))

(defn longest-sequence [a-seq]
  (cond (singleton? a-seq) (first a-seq)
        (empty? a-seq) nil
        :else (seq-max (first a-seq)
                       (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
   (cond
    (empty? a-seq)
     '()
    (pred? (first a-seq))
     (cons (first a-seq)
           (my-take-while pred? (rest a-seq)))
    :else
     '() ))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
    '()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
    true
   (or (empty? a-seq) (empty? b-seq))
    false
   (not (= (first a-seq) (first b-seq)))
    false
   :else
    (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
   1
   (* n (power n (dec k)))))

(defn fib [n]
  (if
   (or (= n 1) (= n 0))
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [k elem]
  (if (<= k 0)
    '()
    (cons elem
          (my-repeat (dec k) elem))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
     (cons '() '())
     (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [init-a-seq (reverse (inits a-seq))
        rev-tails-a-seq (tails a-seq)
        helper (fn [a b] (concat b a))]
  (if (empty? a-seq)
    (list '())
    (rest (map helper init-a-seq rev-tails-a-seq)))))



(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freq (if (contains? freqs (first a-seq))
                     (inc (get freqs (first a-seq)))
                     1)]
    (my-frequencies-helper
     (assoc freqs (first a-seq) new-freq)
     (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [c-pair (first a-map)]
    (concat (repeat (second c-pair) (first c-pair))
            (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [seq-size (count a-seq)
        seq-halve (int (/ seq-size 2))]
    [(my-take seq-halve a-seq) (my-drop seq-halve a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (<= (first a-seq) (first b-seq)) (cons (first a-seq)
                                          (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq)
               (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [halve-seq (halve a-seq)]
    (seq-merge (merge-sort (first halve-seq)) (merge-sort(second halve-seq))))))

(defn monotonics-helper [pred old-elem acc a-seq]
  (cond (empty? a-seq) [acc '()]
        (pred old-elem (first a-seq)) (monotonics-helper pred
                                                 (first a-seq)
                                                 (conj acc (first a-seq))
                                                 (rest a-seq))
        :else [acc a-seq]))

(defn split-into-monotonics [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) [[(first a-seq)]]
        :else
          (let [current-pred (if (> (first a-seq) (second a-seq)) > <)
               [result new-seq] (monotonics-helper current-pred
                                                   (first a-seq)
                                                   [(first a-seq)]
                                                   (rest a-seq))]
           (cons result
                 (split-into-monotonics new-seq)))))

(defn switch [a-ind b-ind a-vec]
  (let [a-val (get a-vec a-ind)
        b-val (get a-vec b-ind)]
  (assoc a-vec a-ind b-val b-ind a-val)))

(defn pair? [a-seq]
  (and
   (not (= (second a-seq) nil))
   (= (second (rest a-seq)) nil)))

(declare permut-main)
(declare permut-helper)

(defn permut-main [a-vec]
  (let [current-rot (rotations a-vec)]
    (map permut-helper current-rot)))

(defn permut-helper [a-seq]
  (let [map-helper (fn [y] (cons (first a-seq) y))]
   (map map-helper (if (pair? (rest a-seq))
                        (rotations (rest a-seq))
                        (apply concat (permut-main (rest a-seq)))))))

(defn permutations [a-set]
 (if (empty? a-set)
   (list '())
  (let [a-vec (vec a-set)]
   (apply concat (permut-main a-vec)))))

(defn right-rotate [a-seq]
   (let [a-vec (vec (rest a-seq))
         left-most (first a-seq)]
     (conj a-vec left-most)))

(defn drop-nth [index a-seq]
  (concat (take index a-seq) (drop (inc index) a-seq)))

(defn powerset-helper [n a-seq]
  (if (= (count a-seq) n)
   '()
    (cons
      (drop-nth n a-seq)
      (powerset-helper (inc n) a-seq))))

(defn powerset-iter [a-set]
  (if (singleton? a-set)
    [(vec a-set)]
    (cons (vec a-set)
          (apply concat (map powerset-iter (powerset-helper 0 a-set))))))

(defn powerset [a-set]
  (set (map set (cons [] (powerset-iter a-set)))))
;
