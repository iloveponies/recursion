(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

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
        (cons (first a-seq)
              (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== (first a-seq) elem) true
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
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1) )
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (> how-many-times 0) (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (cond
    (not (pos? up-to)) '()
    :else (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (seq (set (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [item (first a-seq)
          amt (if (contains? freqs item)
                (get freqs (first a-seq))
                0)
          new-freqs (assoc freqs item (+ amt 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (not (empty? a-map))

    (let [kv-pair (first a-map)
          item (get kv-pair 0)
          amt (get kv-pair 1)]
      (concat (repeat amt item) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (and (> n 0) (> (count coll) 0))
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (and (> n 0) (>= (count coll) 0))
    (my-drop (- n 1) (rest coll))
    coll))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))
        start (my-take mid a-seq)
        end (my-drop mid a-seq)]
    [(if (empty? start)
            '()
            start)
     end]))

(defn seq-merge [a-seq b-seq]
  (let [stack-merge (fn stack-merge [a b c]
                      (cond
                        (empty? a) (concat c b)
                        (empty? b) (concat c a)
                        :else (let [a1 (first a)
                                    b1 (first b)]
                                (if (> a1 b1)
                                  (stack-merge a (rest b) (concat c (cons b1 '())))
                                  (stack-merge (rest a) b (concat c (cons a1 '())))))))]
    (stack-merge a-seq b-seq '())))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (= 1 (count a-seq)) a-seq
    :else (let [halves (halve a-seq)]
            (seq-merge (merge-sort (get halves 0)) (merge-sort (get halves 1))))))

(defn split-into-monotonics [a-seq]
  (cond
    (empty? a-seq) '()
    (= 1 (count a-seq)) (seq a-seq)
    :else (let [increasing? (if (>= (second a-seq) (first a-seq))
                              true
                              false)
                pos-dir? (fn [x] (cond
                                   (empty? x) true
                                   (= 1 (count x)) true
                                   :else (if (> (last x) (last (butlast x)))
                                           true
                                           false)))
                neg-dir? (fn [x] (cond
                                   (empty? x) true
                                   (= 1 (count x)) true
                                   :else (not (pos-dir? x))))
                start (if increasing?
                        (last (take-while pos-dir? (inits a-seq)))
                        (last (take-while neg-dir? (inits a-seq))))
                end (seq (drop (count start) a-seq))]
            (cons start (split-into-monotonics end)))))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (apply concat (map
                    (fn [[x & xs]] (map (fn [y] (cons x y)) (permutations xs)))
                    (rotations a-set)))))


(defn powerset [a-set]
  (set (map set (distinct (map merge-sort (apply concat (map inits (permutations a-set))))))))

