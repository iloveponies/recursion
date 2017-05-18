(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [l1 (count seq-1)
        l2 (count seq-2)]
    (if (> l1 l2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max (first a-seq)
               (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (pred? f)
        (cons f
              (my-filter pred? r))
        (my-filter pred? r)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (pred? f)
        (cons f (my-take-while pred? r))
        '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (pred? f)
        (my-drop-while pred? r)
        a-seq))))

(defn seq= [a-seq b-seq]
  (let [fa (first a-seq)
        fb (first b-seq)
        ra (rest a-seq)
        rb (rest b-seq)]
    (cond
      (and (empty? a-seq)
          (empty? b-seq)) true
      (or (empty? a-seq)
          (empty? b-seq)) false
      (not (== fa fb)) false
      :else (seq= ra rb))))

(defn my-map [f seq-1 seq-2]
  (let [f1 (first seq-1)
        f2 (first seq-2)
        r1 (rest seq-1)
        r2 (rest seq-2)]
    (if (or (empty? seq-1)
            (empty? seq-2))
      '()
      (cons (f f1 f2)
            (my-map f r1 r2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (dec n))
       (fib (- n 2)))))

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
    (cons [] [])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons [] [])
    (reverse (cons a-seq
                   (reverse (inits (butlast a-seq)))))))

(defn rotations [a-seq]
  (let [res-seq (my-map concat (tails a-seq) (inits a-seq))]
    (if (= 1 (count res-seq)) res-seq (rest res-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [f (first a-seq)
          r (rest a-seq)
          fun (fn [k] (= k f))]
      (if (contains? freqs f)
        (my-frequencies-helper freqs r)
        (my-frequencies-helper (assoc freqs f (count(my-filter fun a-seq))) r)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [f (first a-map)
          r (rest a-map)
          value (val f)
          k (key f)]
      (concat (repeat value k) (un-frequencies r)))))

(defn my-take [n coll]
  (let [f (first coll)
        r (rest coll)]
    (if (empty? coll)
      ()
      (if (== n 0)
        ()
        (cons f (my-take (dec n) r))))))

(defn my-drop [n coll]
  (let [f (first coll)
        r (rest coll)]
    (if (empty? coll)
      ()
      (if (<= n 0)
        coll
        (my-drop (dec n) r)))))

(defn halve [a-seq]
  (let [l (count a-seq)
        fhl (int (/ l 2))
        shl (- l fhl)
        fh (my-take fhl a-seq)
        sh (my-drop fhl a-seq)]
    (vector fh sh)))

(defn seq-merge [a-seq b-seq]
  (let [fa (first a-seq)
        fb (first b-seq)
        ra (rest a-seq)
        rb (rest b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< fa fb) (cons fa (seq-merge ra b-seq))
      :else (cons fb (seq-merge rb a-seq)))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

