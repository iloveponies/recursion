(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (empty? (rest coll)) (not (empty? coll))) true false))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (>= (count seq-2) (count seq-1)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (== (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if(or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs
          (if (not (contains? freqs (first a-seq)))
            (assoc freqs (first a-seq) 1)
            (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1)))
          ]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (get (first a-map) 1) (get (first a-map) 0)) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if(empty? coll)
    ()
    (if (== n 0)
      ()
      (cons (first coll) (my-take (dec n) (rest coll))))))

(defn my-drop [n coll]
  (if(empty? coll)
    ()
    (if(== n 0)
      (seq coll)
      (my-drop (dec n) (rest coll)))))

(defn halve [a-seq]
  (let [ndrop (int (/ (count a-seq) 2))]
    (vector (my-take ndrop a-seq) (my-drop ndrop a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (> (first b-seq) (first a-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if(< (count a-seq) 2)
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))))


(defn split-into-monotonics [a-seq]
  (defn monotonic? [a-seq]
    (or (apply <= a-seq) (apply >= a-seq)))
  (if
    (empty? a-seq)
    a-seq
    (let
      [f (fn [x y] (if(monotonic? x)
                    (list x y)
                    (recur (butlast x) (cons (last x) y))))
       [x y] (f a-seq '())]
      (cons x (split-into-monotonics y)))))

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (apply concat (map (fn [x] (map cons (repeat (first x)) (permutations (rest x))))
                  (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
      (clojure.set/union (powerset (next a-set))
        (map #(conj % (first a-set)) (powerset (next a-set))))))

