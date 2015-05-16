(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [fst (first a-seq)]
      (if (singleton? a-seq)
        fst
        (max fst (max-element (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [fst (first a-seq)]
      (if (singleton? a-seq)
        fst
        (seq-max fst (longest-sequence (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [fst (first a-seq)]
      (if (pred? fst)
        (cons fst (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [fst (first a-seq)]
      (if (pred? fst)
        (cons fst (my-take-while pred? (rest a-seq)))
        '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [fst (first a-seq)]
      (if (pred? fst)
        (my-drop-while pred? (rest a-seq))
        a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1 (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (- n 2)) (fib (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons
     what-to-repeat
     (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (let [x (dec up-to)]
      (cons x (my-range x)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (seq (set (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [fst (first a-seq)
          cnt (get freqs fst 0)]
      (my-frequencies-helper
       (assoc freqs fst (inc cnt))
       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [dismantle (fn [pair]
               (let [[elem n] pair]
                 (repeat n elem)))]
    (apply concat (map dismantle a-map))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (empty? coll)
    '()
    (if (zero? n)
      coll
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [n (count a-seq)
        i (int (/ n 2))]
    (vector (my-take i a-seq) (my-drop i a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a (first a-seq)
                b (first b-seq)]
            (if (< a b)
              (cons a (seq-merge (rest a-seq) b-seq))
              (cons b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[head tail] (halve a-seq)]
      (seq-merge (merge-sort head) (merge-sort tail)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn monotonic-seq [a-seq]
  (last (take-while monotonic? (rest (inits a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [next-mono (monotonic-seq a-seq)]
      (cons next-mono (split-into-monotonics (drop (count next-mono) a-seq))))))

(defn permutations [a-set]
  (cond
   (empty? a-set) '(())
   (singleton? a-set) (list (seq a-set))
   :else (for [head a-set
               tail (permutations (disj (set a-set) head))]
           (cons head tail))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (set
     (concat
      (powerset (next a-set))
      (map #(conj % (first a-set)) (powerset (next a-set)))))))


;---------------------
;    Exercise 2
;
;    (product [1 2 4])
;=   (product (cons 1 (cons 2 (cons 4 '()))))
;    (* 1 (product (cons 2 (cons 4 '()))))
;    (* 1 (* 2 (product (cons 4 '()))))
;    (* 1 (* 2 (* 4 (product '()))))
;    (* 1 (* 2 (* 4 1)))
;    (* 1 (* 2 4))
;    (* 1 8)
;    8
