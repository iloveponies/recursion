(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
  (* (first coll)
     (product (rest coll)))))

(defn singleton? [coll]
  (let [[a b] [(first coll) (rest coll)]]
    (if (empty? coll)
      false
    (empty? b))))

(defn my-last [coll]
  (let [[a b] [(first coll) (rest coll)]]
    (if(empty? b)
      a
      (my-last b))))

(defn max-element [a-seq]
  (let [[a b] [(first a-seq) (rest a-seq)]]
    (if (empty? b)
      a
    (max a (max-element b)))))

(defn seq-max [seq-1 seq-2]
  (letfn [(submax [sub1 sub2]
    (cond
      (empty? sub1) seq-2
      (empty? sub2) seq-1
      :else (submax (rest sub1) (rest sub2))))]
    (submax seq-1 seq-2)))


(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
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

(defn seq= [a b]
  (cond
   (and (empty? a) (empty? b)) true
   (empty? a) false
   (empty? b) false
   (= (first a) (first b)) (seq= (rest a) (rest b))
   :else false))

(defn my-map [f a b]
  (cond
    (empty? a) ()
    (empty? b) ()
    :else (cons (f (first a) (first b)) (my-map f (rest a) (rest b)))))


(defn power [n k]
  (cond
   (= k 0) 1
   :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [times what]
  (cond
   (< times 1) ()
   :else  (cons what (my-repeat (- times 1) what))))

(defn my-range [up-to]
  (cond
   (= up-to 0) ()
   :else (conj (my-range (dec up-to)) (dec up-to))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) (list a-seq)
   :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotateby [sq n]
  (cond
   (= n 0) ()
   :else (let [rotation (concat (rest sq) [(first sq)])]
          (concat rotation (rotateby rotation (dec n))))))

(defn rotations [sq]
  (cond
   (empty? sq) (conj ()())
   :else(partition (count sq) (rotateby sq (count sq)))))

(defn my-frequencies-helper [freqs s]
  (letfn [(increment [mappi word]
                     (if (contains? mappi word)
                       (assoc mappi word (inc (get mappi word)))
                       (assoc mappi word 1)))]
  (cond
   (empty? s) freqs
   :else (my-frequencies-helper (increment freqs (first s)) (rest s)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
  (empty? a-map) ()
  :else
   (let [mark (first (first a-map))
        times (first (rest (first a-map)))]

     (cons (repeat times mark) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll)) ()
  (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (> n 0) (my-drop (dec n) (rest coll))
    :else (cons (first coll)
                (my-drop n (rest coll)))))

(defn halve [a-seq]
  (let [n (int (/(count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) ()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (<= (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))


(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    a-seq
    (let [halfs (halve a-seq)
          [x] halfs
          [_ y] halfs]
      (seq-merge (merge-sort x)
                 (merge-sort y)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

