(ns recursion)


(defn nil-seq? [a-seq]
  (or (not (sequential? a-seq)) (empty? a-seq)))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if 
    (or (empty? coll) 
        (not (empty? (rest coll)))) 
    false 
    true))

(defn my-last [coll]
  (cond
    (nil-seq? coll) nil
    (singleton? coll) (first coll)
    :default (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (nil-seq? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :default (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  "Return the longer one of seq-1 and seq-2. If same size, return seq-2"
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (cond 
    (nil-seq? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :default (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (not (= elem (first a-seq))) (sequence-contains? elem (rest a-seq))
    :default true))

(defn my-take-while [pred? a-seq]
  (let [first-item (first a-seq)]
    (if (and (not (empty? a-seq)) (pred? first-item))
      (cons first-item (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (and (not (empty? a-seq)) (pred? (first a-seq)))
    (my-drop-while pred? (rest a-seq))
    a-seq))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (and (not (or (empty? a-seq) 
                  (empty? b-seq))) 
         (= (first a-seq) (first b-seq)) 
         (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<  n 2)
    n
    (+ (fib (dec (dec n))) (fib (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (let [n (dec up-to)]
      (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
      (rest (map concat (tails a-seq) (reverse (inits a-seq))))))


(defn my-frequencies-helper [freqs a-seq]
  (let [first-elem (first a-seq)
        rest-elems (rest a-seq)
        a-freq (get freqs first-elem)]
    (cond
      (empty? a-seq) freqs
      (contains? freqs first-elem) (my-frequencies-helper (assoc freqs first-elem (inc a-freq)) rest-elems)
      :default  (my-frequencies-helper (assoc freqs first-elem 1) rest-elems))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [first-pair (first a-map)
         [what-to-repeat how-many-times] (seq first-pair)]
      (concat (my-repeat how-many-times what-to-repeat) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half (seq a-seq)) (my-drop half (seq a-seq))]))
  

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)
        rest-a (rest a-seq)
        rest-b (rest b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a b) (cons a (seq-merge rest-a b-seq))
      :default (cons b (seq-merge a-seq rest-b)))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) '()
    (= (count a-seq) 1) a-seq
    :default (let [[first-half second-half] (halve a-seq)]
      (seq-merge (merge-sort first-half) (merge-sort second-half)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    false
    (or 
      (apply <= a-seq)
      (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic-begin (first (filter monotonic? (inits a-seq)))
          rest-of-seq (my-drop (count monotonic-begin) a-seq)]
      (cons monotonic-begin (split-into-monotonics rest-of-seq)))))


(defn permutations [a-set]
  a-set)

(defn powerset [a-set]
  a-set)

