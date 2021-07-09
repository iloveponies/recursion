(ns recursion)

(defn product [coll]
  (if (empty? coll) 
     1
     (* (first coll)
        (product (rest coll)))))

(defn singleton? [coll]
  (and 
    (not (empty? coll)) 
    (empty? (rest coll))))

(defn my-last [coll]
  (cond 
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond 
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq)
               (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2)) seq-2 seq-1))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq)
                   (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      ())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
    (cons (mapv identity a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '([])
    (cons (mapv identity a-seq) (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '([])
    (rest (map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-item (first a-seq)
          new-count (if (contains? freqs new-item)
                      (inc (get freqs new-item))
                      1)
          new-freqs (assoc freqs new-item new-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [elems (keys a-map)
        freqs (vals a-map)
        pairs (map vector freqs elems)
        repeater (fn [[n x]] (repeat n x))]
    (apply concat (map repeater pairs))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll)) 
    '()
    (let [new-n (dec n)
          item  (first coll)]
      (cons item (my-take new-n (rest coll))))))

(defn my-drop [n coll]
  (if (or (= n 0) (empty? coll))
    coll
    (let [new-n (dec n)
          remaining (rest coll)]
      (my-drop new-n remaining))))

(defn halve [a-seq]
  (let [midpoint (int (/ (count a-seq) 2)) 
        first-half (my-take midpoint a-seq)
        second-half (my-drop midpoint a-seq)]
    [first-half second-half]))

(defn seq-merge [a-seq b-seq]
  (let [first-a-seq (first a-seq)
        first-b-seq (first b-seq)]
    (cond
      (and (empty? a-seq) (empty? b-seq)) '()
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< first-a-seq first-b-seq) (cons first-a-seq (seq-merge (rest a-seq) b-seq))
      (> first-a-seq first-b-seq) (cons first-b-seq (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond
    (empty? (rest a-seq)) a-seq
    :else (apply seq-merge (map merge-sort (halve a-seq)))))

(defn monotonic? [a-seq]
  (cond 
    (< (count a-seq) 2) false
    :else (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (let [inits-seq (reverse (inits a-seq)) 
        not-monotonic? (complement monotonic?)
        monotonics (drop-while not-monotonic? inits-seq)]
;    (println "---" a-seq)
    (cond
      (empty? monotonics) '()
      :else (let [last-m (first (reverse (filter monotonic? monotonics)))
                  n (count last-m)]
;              (println "+++" last-m "++++" monotonics)
              (cons last-m (split-into-monotonics (drop n a-seq)))))))

(split-into-monotonics [0 5 4 7 1 3])

(defn permutations [a-set]
  (cond
    (empty? a-set) '(())
    (singleton? a-set) (cons (seq a-set) '())
    :else (for [head a-set
                tail (permutations (disj (set a-set) head))]
;            (println head)
            (cons head tail))))

(defn powerset [a-set]
  (cond
    (empty? a-set) #{#{}}
    :else (set (conj 
                 (for [item a-set
                       tail (powerset (disj (set a-set) item))]
                   (set tail))
                   (set a-set)))))
