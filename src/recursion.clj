(ns recursion)

(defn product [coll]
 (if (empty? coll)
  1
  (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
 (and (empty? (rest coll))
      (not (empty? coll))))

(defn my-last [coll]
 (cond
  (empty? coll) nil
  (singleton? coll) (first coll)
  :else (my-last (rest coll))))

(defn max-element [a-seq]
 (cond
  (empty? a-seq) nil
  (singleton? a-seq) (first a-seq)
  :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
 (let [length-seq-1 (count seq-1)
       length-seq-2 (count seq-2)]
  (if (> length-seq-1 length-seq-2)
   seq-1
   seq-2)))

(defn longest-sequence [a-seq]
 (cond
  (empty? a-seq) nil
  (singleton? a-seq) (first a-seq)
  :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
 (if (empty? a-seq)
  a-seq
  (let [first-element (first a-seq)
        rest-of-sequence (rest a-seq)]
   (if (pred? first-element)
    (cons first-element (my-filter pred? rest-of-sequence))
    (my-filter pred? rest-of-sequence)))))

(defn sequence-contains? [elem a-seq]
 (cond
  (empty? a-seq) false
  (not (= (first a-seq) elem)) (sequence-contains? elem (rest a-seq))
  :else true))

(defn my-take-while [pred? a-seq]
 (cond
  (empty? a-seq) a-seq
  (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
  :else '()))

(defn my-drop-while [pred? a-seq]
 (cond
  (empty? a-seq) a-seq
  (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
  :else (cons (first a-seq) (rest a-seq))))

(defn seq= [a-seq b-seq]
 (cond
  (not (= (count a-seq) (count b-seq))) false
  (and (empty? a-seq) (empty? b-seq)) true
  (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
  :else false))

(defn my-map [f seq-1 seq-2]
 (if (or (empty? seq-1) (empty? seq-2))
  '()
  (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
 (if (zero? k)
  1
  (* n (power n (dec k)))))

(defn fib [n]
 (if (< n 2)
  n
  (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
 (if (< how-many-times 1)
  '()
  (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
 (if (= 0 up-to)
  '()
  (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
 (if (empty? a-seq)
  '(())
  (cons (apply list a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
 (map reverse (tails (reverse a-seq))))

(defn rotations-helper [pending a-seq]
 (if (= pending 0)
  '()
  (let [rotated-seq (concat (rest a-seq) [(first a-seq)])]
   (cons rotated-seq (rotations-helper (dec pending) rotated-seq)))))

(defn rotations [a-seq]
 (let [n-of-elements (count a-seq)]
  (if (= 0 n-of-elements)
   '(())
   (rotations-helper n-of-elements a-seq))))

(defn my-frequencies-helper [freqs a-seq]
 (if (empty? a-seq)
  freqs
  (let [elem (first a-seq)
        elem-count (freqs elem)
        rest-a-seq (rest a-seq)]
   (if (not (nil? elem-count))
    (my-frequencies-helper (conj freqs [elem (inc elem-count)]) rest-a-seq)
    (my-frequencies-helper (conj freqs [elem 1]) rest-a-seq)))))

(defn my-frequencies [a-seq]
 (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
 (if (empty? a-map)
  '()
  (let [[elem times-to-repeat] (first a-map)
        rest-of-map (rest a-map)
        repetition-seq (repeat times-to-repeat elem)]
   (concat repetition-seq (un-frequencies rest-of-map)))))

(defn my-take [n coll]
 (if (or (empty? coll) (= n 0))
  '()
  (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
 (if (or (empty? coll) (= n 0))
  coll
  (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
 (let [half-index (int (/ (count a-seq) 2))]
  [(my-take half-index a-seq) (my-drop half-index a-seq)]))

(defn seq-merge [a-seq b-seq]
 (cond
  (empty? a-seq) b-seq
  (empty? b-seq) a-seq
  :else (let [first-a (first a-seq)
              rest-a (rest a-seq)
              first-b (first b-seq)
              rest-b (rest b-seq)]
         (if (< first-a first-b)
          (cons first-a (seq-merge rest-a b-seq))
          (cons first-b (seq-merge a-seq rest-b))))))

(defn merge-sort [a-seq]
 (if (< (count a-seq) 2)
  a-seq
  (let [[first-half second-half] (halve a-seq)]
   (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
