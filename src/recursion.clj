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
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (let [current (first a-seq)]
    (if (singleton? a-seq)
      current
      (if (empty? a-seq)
        nil
        (max current
             (max-element (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (let [seq-size (fn ! [my-seq]
                   (if (empty? my-seq)
                     0
                     (+ 1 (! (rest my-seq)))))]
    (if (> (seq-size seq-1) (seq-size seq-2))
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (let [current (first a-seq)]
    (if (singleton? a-seq)
      current
      (if (empty? a-seq)
        nil
        (seq-max current
                 (longest-sequence (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (let [current (first a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? current)
        (cons current (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [current (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? current) (cons current (my-take-while pred? (rest a-seq)))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (or
    (and (empty? a-seq) (empty? b-seq))
    (and
      (= (first a-seq)
         (first b-seq))
      (seq= (rest a-seq)
            (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (<= k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (let [fib_ (fn ! [x y z]
               (cond
                 (== x 0) y
                 (== x 1) z
                 :else (! (- x 1) z (+ y z))))]
    (fib_ n 0 1)))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))


(defn my-range [up-to]
  (let [current (- up-to 1)]
    (if (<= up-to 0)
      '()
      (cons current (my-range current)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [rotate (fn [some-seq]
                 (if (empty? some-seq)
                   '()
                   (concat (rest some-seq) (cons (first some-seq) '()))))
        rotations_ (fn ! [b-seq]
                     (let [new-seq (cons b-seq '())]
                       (if (seq= (seq a-seq) b-seq)
                         new-seq
                         (concat new-seq (! (rotate b-seq))))))]
    (rotations_ (rotate a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [current (first a-seq)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper (assoc freqs current (if (contains? freqs current)
                                                    (+ 1 (freqs current))
                                                    1)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [current (first a-map)
          k (get current 0)
          v (get current 1)]
      (concat (my-repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll)
          (<= n 0))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (<= n 0) (seq coll)
    :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [split (int (/ (count a-seq) 2))]
    (cons (my-take split a-seq) (cons (my-drop split a-seq) '()))))

(defn seq-merge [a-seq b-seq]
  [:-])

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

