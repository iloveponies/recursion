(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (boolean (seq coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq)
          a-seq
        (pred? (first a-seq))
          (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else
          (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= (first a-seq) elem) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq))
          (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq))
          (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (dec n))
       (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (butlast (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first (first a-seq)
          rest( rest a-seq)
          new-freqs (assoc freqs first (inc (get freqs first 0)))]
      (my-frequencies-helper new-freqs rest))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[val count]]
                       (repeat count val))
                     a-map)))

(defn my-take [n coll]
  (if (or (<= n 0)
          (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0)
          (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    [(my-take middle a-seq)
     (my-drop middle a-seq)]))

(defn seq-merge [a-seq b-seq]
 (if (or (empty? a-seq)
         (empty? b-seq))
   ; if one is empty this will return just the nonempty one
   (concat a-seq b-seq)
   (let [a (first a-seq)
         b (first b-seq)]
     (if (<= a b)
       (cons a (seq-merge (rest a-seq) b-seq))
       (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (->> a-seq
         halve
         (map merge-sort)
         (apply seq-merge))))

(defn monotonic? [a-seq]
  "return true if a-seq is monotonically increasing or decreasing"
  (or (empty? a-seq)
      (apply > a-seq)
      (apply < a-seq)))

(defn split-into-monotonics [a-seq]
  (if (or (empty? a-seq)
          (singleton? a-seq))
    a-seq
    (let [seq-inits (inits a-seq)
          seq-monotonics (take-while monotonic? seq-inits)
          largest (last seq-monotonics)
          size (count largest)
          rest (drop size a-seq)]
      (cons largest
            (split-into-monotonics rest)))))

(defn intersperse [val a-seq]
  "a sequence created by adding val at each point in a-seq"
  (map #(concat (take % a-seq) [val] (drop % a-seq))
       (range (inc (count a-seq)))))

(defn permutations [a-set]
  (cond (empty? a-set) '(())
        (singleton? a-set) (list (seq a-set))
        :else (let [val (first a-set)
                    done (permutations (rest a-set))]
                (mapcat #(intersperse val %) done))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [done (powerset (rest a-set))
          val  (first a-set)]
      (apply conj done (map #(conj % val) done)))))

