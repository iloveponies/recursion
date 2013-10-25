(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
        (product (rest coll)))))

(defn singleton? [coll]
  (and (not (nil? (first coll))) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (not (empty? a-seq))
    (max 
      (first a-seq) 
      (if (= (max-element (rest a-seq)) nil)
        0
        (max-element (rest a-seq))))
    nil))

(defn seq-max [seq-1 seq-2]
  (if (empty? (rest seq-1))
    seq-2
    (if(empty? (rest seq-2))
      seq-1
      (let [asdf (seq-max (rest seq-1) (rest seq-2))]
        (if (= (rest seq-1) asdf)
          (cons (first seq-1) asdf)
          (cons (first seq-2) asdf))))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (longest-sequence (cons (seq-max (first a-seq) (second a-seq)) (rest (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (and (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (zero? n)
    0
    (if (= n 1)
      1
      (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons [] ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotation-helper [n a-seq]
  (if (zero? n)
    ()
    (let [new-jutskeli (concat (rest a-seq) (vector (first a-seq)))]
      (cons new-jutskeli (rotation-helper (dec n) new-jutskeli)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (rotation-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [eka (first a-seq)
          loput (rest a-seq)]
      (if (contains? freqs eka)
        (my-frequencies-helper (assoc freqs eka (inc (get freqs eka))) loput)
        (my-frequencies-helper (assoc freqs eka 1) loput)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [eka (first a-map)
          loput (rest a-map)]
      (concat (repeat (second eka) (first eka)) (un-frequencies loput)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    (cons (my-take half a-seq) (cons (my-drop half a-seq) ()))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [size (count a-seq)
        half (int (/ size 2))]
    (cond
     (empty? a-seq) a-seq
     (= size 1) a-seq
     (> size 1) (let [alku (first (halve a-seq))
                      loppu (second (halve a-seq))]
                  (seq-merge (merge-sort alku) (merge-sort loppu))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

