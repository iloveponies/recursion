(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))



(defn singleton? [coll]
  (let [second-part (rest coll)]
    (if (or (empty? coll) (not (empty? second-part))) false true)))


(defn my-last [coll]
  (let [first-part (first coll)
        last-part (rest coll)]
    (if (empty? last-part) first-part (my-last last-part))))



;(defn max-element [a-seq]
;  (if (empty? a-seq) nil (apply max a-seq)))


(defn max-element [a-seq]
  (let [first-element (first a-seq)
        rest-elements (rest a-seq)]
    (if (empty? rest-elements) first-element (max first-element (max-element rest-elements)))))



(defn seq-max [seq-1 seq-2]
  (let [seq-1-length (count seq-1)
        seq-2-length (count seq-2)]
    (if (== seq-1-length seq-2-length) seq-2 (if (< seq-1-length seq-2-length) seq-2 seq-1))))



(defn longest-sequence [a-seq]
  (let [first-seq (first a-seq)
        rest-seq (rest a-seq)]
    (if (empty? rest-seq) first-seq (seq-max first-seq (longest-sequence rest-seq)))))




(defn my-filter [pred? a-seq]
  (let [first-item (first a-seq)
        rest-items (rest a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? first-item)
        (cons first-item (my-filter pred? rest-items))
        (my-filter pred? rest-items)))))



(defn sequence-contains? [elem a-seq]
   (if (empty? a-seq)
     false
     (if (= elem (first a-seq))
       true
       (sequence-contains? elem (rest a-seq)))))



(defn my-take-while [pred? a-seq]
  (let [first-i (first a-seq)
        rest-i (rest a-seq)]
    (if (empty? a-seq)
      '()
      (if (not (pred? first-i))
        '()
        (cons first-i (my-take-while pred? rest-i))))))


(defn my-drop-while [pred? a-seq]
  (let [first-item (first a-seq)
        rest-items (rest a-seq)]
    (if (empty? a-seq)
      '()
      (if (pred? first-item)
        (my-drop-while pred? rest-items)
        a-seq))))



(defn seq= [a-seq b-seq]
  (let [fsa (first a-seq)
        fsb (first b-seq)
        rest-a (rest a-seq)
        rest-b (rest b-seq)
        rest-a-empty-rest-b-not (and (empty? rest-a) (not (empty? rest-b)))
        rest-b-empty-rest-a-not (and (empty? rest-b) (not (empty? rest-a)))]
    (if (and (empty? a-seq) (empty? b-seq))
      true
      (if (or rest-a-empty-rest-b-not rest-b-empty-rest-a-not)
        false
        (if (= fsa fsb)
          (seq= rest-a rest-b)
          false)))))



(defn my-map [f seq-1 seq-2]
  (let [first-1 (first seq-1)
        first-2 (first seq-2)
        rest-1 (rest seq-1)
        rest-2 (rest seq-2)]
    (if (or (empty? seq-1) (empty? seq-2))
      '()
      (cons (f first-1 first-2) (my-map f rest-1 rest-2)))))



(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))



(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))




(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))



(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))



(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))



(defn rotations [a-seq]
  (let [tail (tails a-seq)
        ini (reverse (inits a-seq))
        rotation (map concat tail ini)]
    (if (empty? a-seq)
      '(())
      (rest rotation))))




(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-item (first a-seq)
          count-value (get freqs first-item)
          new-freqs (if (nil? count-value)
                      (assoc freqs first-item 1)
                      (assoc freqs first-item (+ count-value 1)))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))



(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

(defn halve [a-seq]
  [:-])

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

