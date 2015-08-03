(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

; Reference to sub isn't allowed within itself; hence giving self as a parameter.
(defn my-last [coll]
  (let [sub (fn [f x] (if (singleton? x) (first x) (f f (rest x))))]
    (if (empty? coll)
      nil
      (sub sub coll))))

(defn max-element [a-seq]
  (let [sub (fn [f x] (if (singleton? x) (first x) (max (first x) (f f (rest x)))))]
    (if (empty? a-seq)
      nil
      (sub sub a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [sub (fn [f x] (if (singleton? x) (first x) (seq-max (first x) (f f (rest x)))))]
    (if (empty? a-seq)
      nil
      (sub sub a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [
          x (first a-seq)
          f (fn [] (my-filter pred? (rest a-seq)))
          ]
      (if (pred? x) (cons x (f)) (f)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [x (first a-seq)]
      (if (pred? x)
        (cons x (my-take-while pred? (rest a-seq)))
        '()))))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a b]
  (cond
   (and (empty? a) (empty? b)) true
   (or (empty? a) (empty? b)) false
   (not (= (first a) (first b))) false
   :else (seq= (rest a) (rest b))))

(defn my-map [f a b]
  (if (or (empty? a) (empty? b))
    '()
    (cons (f (first a) (first b)) (my-map f (rest a) (rest b)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [n what-to-repeat]
  (if (<= n 0)
    '()
    (cons what-to-repeat (my-repeat (- n 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [x]
  (cons (sequence x)
        (if (empty? x) '() (tails (rest x)))))

(defn inits [x]
  (map reverse (tails (reverse x))))

(defn rotations [x]
  (let [
        c (count x)
        sub (fn [f y n]
              (let [
                    z (first y)
                    rotated (concat (rest y) (seq [z]))
                    ]
                (cons
                 rotated
                 (if (< 1 n)
                   (f f rotated (- n 1))
                   '()))))
              ]
    (if (= 0 c)
      '()
      (sub sub x c))))

(defn my-frequencies [a-seq]
  (let [
        helper (fn [f, as, freqs]
                 (if (empty? as)
                   freqs
                   (let [
                         k (first as)
                         new-freqs (assoc freqs k (inc (get freqs k 0)))
                         ]
                     (f f (rest as) new-freqs))))
        ]
    (helper helper a-seq {})))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [
          [v c] (first a-map)
          ]
      (concat (repeat c v) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (and (< 0 n) (not (empty? coll)))
    (concat (seq [(first coll)]) (my-take (- n 1) (rest coll)))
    '()))

(defn my-drop [n coll]
  (if (and (< 0 n) (not (empty? coll)))
    (my-drop (- n 1) (rest coll))
    coll))

(defn halve [a-seq]
  (let [
        c (count a-seq)
        n (int (/ c 2))
        ]
    [(my-take n a-seq) (my-drop n a-seq)]))


(defn seq-merge [a-seq b-seq]
  (if (not (or (empty? a-seq) (empty? b-seq)))
    (let [
          a (first a-seq)
          b (first b-seq)
          ]
      (if (<= a b)
        (cons a (seq-merge (rest a-seq) b-seq))
        (cons b (seq-merge a-seq (rest b-seq)))))
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     :else '())))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (singleton? a-seq) a-seq
   :else (let [
               [a b] (halve a-seq)
               ]
           (seq-merge
            (merge-sort a)
            (merge-sort b)))))


(defn my-drop-while-bin [binop a-seq n]
  (let [
        a1 (first a-seq)
        r1 (rest a-seq)
        a2 (first r1)
        r2 (rest r1)
        ]
      (cond
       (empty? a-seq) [a-seq n]
       (empty? r1) [a-seq n]
       (binop a1 a2) (my-drop-while-bin binop r1 (inc n))
       :else [r1 n])))


(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [
          [ra ca] (my-drop-while-bin <= a-seq 1)
          [rb cb] (my-drop-while-bin >= a-seq 1)
          ]
      (if (>= ca cb)
        (cons (take ca a-seq) (split-into-monotonics (drop ca a-seq)))
        (cons (take cb a-seq) (split-into-monotonics (drop cb a-seq)))))))

(defn rotate-char [ch a-seq]
  (let [sub (fn [f x s n] (let [
                                head (take n s)
                                tail (drop n s)
                                ]
                            (if (empty? tail)
                              (cons (concat s [x]) '()) ;I couldn't come up with a way to declare a sequence by itself.
                              (cons
                               (concat (take n s) (cons x (drop n s)))
                               (f f x s (inc n))))))]
    (sub sub ch a-seq 0)))


(defn permutations-2 [first-item a-set res]
  (let [
        add-char (fn [ch, seqs]
                   (map (fn [x] (rotate-char ch x)) seqs))
        new-res (apply concat (add-char first-item res))
        ]
    (if (empty? a-set)
      new-res
      (permutations-2 (first a-set) (rest a-set) new-res))))


(defn permutations [a-set]
  (if (empty? a-set)
    '()
    (let [
          c1 (first a-set)
          r1 (rest a-set)
          ]
      (if (empty? r1)
        [c1]
        (permutations-2 (first r1) (rest r1) [[c1]])))))


(defn powerset [a-set]
  (let [
        sub (fn [f, as, res]
              (if (empty? as)
                res
                (let [
                      new-res (map (fn [z] (set (concat #{(first as)} z))) res)
                      ]
                  (f f (rest as) (concat res new-res)))))
        ]
    (sub sub a-set #{#{}})))
