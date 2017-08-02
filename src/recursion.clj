(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [left-over (rest a-seq)]
    (if (empty? left-over)
      (first a-seq)
      (max (first a-seq) (max-element left-over)))))

(defn seq-max [seq-1 seq-2]
  (cond
    (>= (count seq-2) (count seq-1)) seq-2
     :else seq-1))

(defn longest-sequence [a-seq]
  (let [left-over (rest a-seq)]
    (if (empty? left-over)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence left-over)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else
      (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (not (= (first a-seq) elem))
      (sequence-contains? elem (rest a-seq))
    :else
      true))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (not (pred? (first a-seq)))
      a-seq
    :else
      (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (empty? a-seq)
      (if (empty? b-seq)
        true
        false)
    (empty? b-seq)
      (if (empty? a-seq)
        true
        false)
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (> (count seq-2) (count seq-1))
      (if (empty? seq-1)
        seq-1
        (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))))
    :else
      (if (empty? seq-2)
        seq-2
        (cons (f (first seq-2) (first seq-1)) (my-map f (rest seq-1) (rest seq-2))))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1)
      []
    :else
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (< up-to 1)
      []
    :else
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
      (cons a-seq [])
    :else
      (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
    (empty? a-seq)
      (cons a-seq [])
    :else
      (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotate [c a-seq]
  (if (= c 0)
    ()
    (cons a-seq (rotate (dec c) (cons (last a-seq) (butlast a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [()]
    (rotate (count a-seq) a-seq)))

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    (contains? freqs (first a-seq)) (my-frequencies-helper freqs (rest a-seq))
    :else (my-frequencies-helper (assoc freqs (first a-seq) (count-elem (first a-seq) a-seq)) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
    (or (= n 0) (empty? coll)) ()
    :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (or (= n 0) (empty? coll)) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split (int(/ (count a-seq) 2))]
    [(my-take split a-seq)(my-drop split a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) ()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (< (count a-seq) 2) a-seq
    :else
      (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (last (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

