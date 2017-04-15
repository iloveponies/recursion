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
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-rec [m a-seq]
  (if (empty? a-seq)
    m
    (max m (max-rec (first a-seq) (rest a-seq)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (max-rec (first a-seq) (rest a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn seq-rec [s a-seq]
  (if (empty? a-seq)
    s
    (seq-max s (seq-rec (first a-seq) (rest a-seq)))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-rec (first a-seq) (rest a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (let [s (seq a-seq)]
    (if (empty? s)
      (cons () ())
      (cons s (tails (rest s))))))

(defn ini [a-seq]
  (let [s (seq a-seq)]
    (if (empty? s)
      (cons () ())
      (cons (reverse s) (ini (rest s))))))

(defn inits [a-seq]
  (ini (reverse a-seq)))

(defn my-split [i n head tail]
  (let [h (conj head (first tail))]
    (if (== i (dec n))
      (concat (rest tail) (seq h))
      (my-split (inc i) n (vec h) (vec (rest tail))))))

(defn rot-help [n a-seq]
  (if (== n 0)
    (cons (seq a-seq) ())
    (cons (my-split 0 n [] (vec a-seq)) (rot-help (dec n) a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (rot-help (dec(count a-seq)) a-seq)))

(defn count-by [a-seq elem i n]
  (if (== i 0)
    n
    (if (= elem (get (vec a-seq) (dec i)))
      (count-by a-seq elem (dec i) (inc n))
      (count-by a-seq elem (dec i) n))))

(defn my-frequencies-helper [a-seq a-set]
  (if (empty? a-set)
    {}
    (let [elem (first a-set)]
      (assoc (my-frequencies-helper a-seq (rest a-set)) elem (count-by a-seq elem (count a-seq) 0)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper a-seq (set a-seq)))

(defn un-helper [k v]
  (if (empty? k)
    ()
    (concat (repeat (first v) (first k)) (un-helper (rest k) (rest v)))))

(defn un-frequencies [a-map]
  (un-helper (seq (keys a-map)) (seq (vals a-map))))

(defn take-helper [i n coll]
  (if (== i n)
    ()
    (cons (first coll) (take-helper (inc i) n (rest coll)))))

(defn my-take [n coll]
  (if (>= n (count coll))
    (take-helper 0 (count coll) coll)
    (take-helper 0 n coll)))

(defn drop-helper [i n coll]
  (if (== i n)
    coll
    (drop-helper (inc i) n (rest coll))))

(defn my-drop [n coll]
  (if (>= n (count coll))
    ()
    (drop-helper 0 n coll)))

(defn halve [a-seq]
  (let [i (int (/ (count a-seq) 2))]
    (vector (my-take i a-seq) (my-drop i a-seq))))

(defn merge-helper [n a-seq i]
  (if (>= n (get (vec a-seq) (dec (count a-seq))))
    (seq (conj (vec a-seq) n))
    (if (< n (get (vec a-seq) i))
      (concat (my-take i a-seq) (cons n (my-drop i a-seq)))
      (merge-helper n a-seq (inc i)))))

(defn seq-merge [a-seq b-seq]
  (if (empty? a-seq)
    (seq b-seq)
    (seq-merge (rest a-seq) (merge-helper (first a-seq) b-seq 0))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (apply <= (vec a-seq)) (<= (count a-seq) 1))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])


















