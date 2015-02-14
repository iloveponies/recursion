(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
   (empty? coll)        false
   (empty? (rest coll)) true
   :else                false))

(defn my-last [coll]
  (cond
   (empty?     coll) nil
   (singleton? coll) (first coll)
   :else             (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty?     a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else              (max (first a-seq)
                           (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty?     a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else              (seq-max (first a-seq)
                               (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-filter pred? (rest a-seq)))
    :else                 (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)          false
   (== elem (first a-seq)) true
   :else                   (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else                 '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)        a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else                 a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
                    true
    (empty? a-seq)  false
    (empty? b-seq)  false
    :else           (and (== (first a-seq) (first b-seq))
                         (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (let [x1 (first seq-1)
          x2 (first seq-2)
          fx (f x1 x2)]
      (cons fx
            (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (if (<= k 0) 1
      (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (<= n 0)      0
    (or (== n 1)
        (== n 2)) 1
    :else (let [fn-2 (fib (- n 2))
                fn-1 (fib (- n 1))]
            (+ fn-1 fn-2))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (- how-many-times 1)
                     what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (let [n-1 (- up-to 1)]
      (cons n-1 (my-range n-1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse
       (tails (reverse  a-seq))))

(defn rotations [a-seq]
  (let [helper
        (fn [xs xss]
          (cond
            (empty? xs)      [[]]
            (== (count xs)
                (count xss)) xss
            :else (let [xs'  (concat (rest xs) [(first xs)])
                        xss' (cons xs' xss)]
                    (recur xs' xss'))))]
    (helper a-seq [])))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key    (first a-seq)
          val    (get freqs key)
          val'   (+ 1 (if val val 0))
          freqs' (assoc freqs key val')
          a-seq' (rest a-seq)]
      (my-frequencies-helper freqs' a-seq'))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[x n] (first a-map)]
      (concat (repeat n x)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll)
          (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [sz (count a-seq)
        md (quot sz 2)]
    [(my-take md a-seq)
     (my-drop md a-seq)]))


(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
      (let [a (first a-seq)
            b (first b-seq)]
        (if (< a b)
          (cons a (seq-merge (rest a-seq) b-seq))
          (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[xs ys] (halve a-seq)
          xs'     (merge-sort xs)
          ys'     (merge-sort ys)]
      (seq-merge xs' ys'))))

(defn mono? [xs]
  (or (apply <= xs)
      (apply >= xs)))

(defn split-into-mono-helper [xs mono monos]
  (if (empty? xs)
    (if (empty? mono) monos
        (concat monos [mono]))
    (let [mono' (concat mono [(first xs)])]
      (if (mono? mono')
        (recur (rest xs) mono' monos)
        (recur (rest xs)
               [(first xs)]
               (concat monos [mono]))))))

(defn split-into-monotonics [a-seq]
  (split-into-mono-helper a-seq [] []))

(defn permute-x [x left right xss]
  (if (empty? right)
    (cons (concat left [x])
          xss)
    (let [perm   (concat left [x] right)
          xss'   (cons perm xss)
          left'  (concat left [(first right)])
          right' (rest right)]
      (recur x left' right' xss'))))

(defn permutations [a-set]
  (if (empty? a-set)
    [[]]
    (let [x    (first a-set)
          prms (permutations (rest a-set))
          f    (fn [right] (permute-x x [] right []))]
      (apply concat
             (map f prms)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [x  (first a-set)
          ps (powerset (rest a-set))
          f  (fn [xs] [(set (cons x xs)) xs])]
      (set (apply concat (map f ps))))))
