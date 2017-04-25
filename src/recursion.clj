(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (defn count-fun[coll, counter]
    (if (empty? coll)
      (- counter 1)
      (count-fun (rest coll)
                 (+ counter 1))))
  (let [depth (count-fun coll 1)]
    (== 1 depth)
    ))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (= (my-last (rest coll)) nil)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [next-elem (max-element (rest a-seq))
          this-elem (first a-seq)]
      (if (= next-elem nil)
        this-elem
        (max this-elem next-elem)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (<= c1 c2)
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (let [next-elem (longest-sequence (rest a-seq))]
      (seq-max (first a-seq) next-elem))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else []))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (= (first a-seq) (first b-seq)) (cond
                                      (and (not (empty? a-seq))
                                           (not (empty? b-seq)))
                                      (seq= (rest a-seq) (rest b-seq))
                                      :else false)
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat
          (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    []
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rot-helper [ts is]
  (if (== (count ts) 1)
    ()
    (cons (concat (first ts) (first is))
          (rot-helper (rest ts) (rest is)))))

(defn rotations [a-seq]
  (let [tls (tails a-seq)
        ins (reverse (inits a-seq))]
    (if (== (count a-seq) 0)
      (seq [()])
      (rot-helper tls ins))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [firsty (first a-seq)
          firsty-val (get freqs firsty)]
      (if firsty-val
        (my-frequencies-helper (assoc freqs firsty (inc firsty-val))
                               (rest a-seq))
        (my-frequencies-helper (assoc freqs firsty 1)
                               (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [k-v (first a-map)
          k (get k-v 0)
          v (get k-v 1)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    []
    (concat [(first coll)] (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n)
          (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [seq-len (count a-seq)
        middle (int (/ seq-len 2))]
    [(my-take middle a-seq) (my-drop middle a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [aempty (empty? a-seq)
        bempty (empty? b-seq)
        ekaa (first a-seq)
        ekab (first b-seq)]
    (cond
      (and aempty bempty) []
      aempty (concat [ekab]
                     (seq-merge a-seq (rest b-seq)))
      bempty (concat [ekaa]
                     (seq-merge (rest a-seq) b-seq))
      (<= ekaa ekab) (concat [ekaa] (seq-merge (rest a-seq) b-seq))
      :else (concat [ekab] (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [halves (halve a-seq)
          left (get halves 0)
          right (get halves 1)]
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

