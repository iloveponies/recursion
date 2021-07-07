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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond 
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond 
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) 
                                (my-take-while pred? (rest a-seq)))
    :else '()))

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
    :esle false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    '()
    (cons (f (first seq-1) (first seq-2)) 
          (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) n
    (== n 1) n
    :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) 
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))


(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '([])
    (cons a-seq (inits (drop-last 1 a-seq)))))

(defn do-rotations [len to-do a-seq]
  (if (zero? to-do)
    '()
    (cons (take len a-seq) (do-rotations len (dec to-do) (rest a-seq)))))

(defn rotations [a-seq]
  (let [len (count a-seq)
        b-seq (concat (rest a-seq) a-seq)]
    (if (zero? len)
      '([])
      (do-rotations len len b-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    (contains? freqs (first a-seq)) 
      (my-frequencies-helper (assoc freqs (first a-seq) 
                                    (+ (get freqs (first a-seq)) 1))
                             (rest a-seq))
    :else (my-frequencies-helper (assoc freqs (first a-seq) 1)
                                 (rest a-seq))))
                                                                   


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) '()
    (singleton? a-map) (repeat (val (first a-map)) (key (first a-map)))
    :else (concat (repeat (val (first a-map)) (key (first a-map)))
                  (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [a-len (int (/ (count a-seq) 2))]
    [(my-take a-len a-seq) (my-drop a-len a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
    (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    (< (first a-seq) (first b-seq)) (cons (first a-seq) 
                                          (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn take-monotonics [pred? a-seq]
  (cond
    (singleton? a-seq) (cons (first a-seq) '())
    (pred? (first a-seq) (first (rest a-seq)))
      (cons (first a-seq) (take-monotonics pred? (rest a-seq)))
    :else (cons (first a-seq) '())))

(defn determine-monotonics [a-seq]
  (cond 
    (singleton? a-seq) (cons (first a-seq) '())
    (< (first a-seq) (first (rest a-seq))) 
      (cons (first a-seq) (take-monotonics < (rest a-seq)))
    (> (first a-seq) (first (rest a-seq)))
      (cons (first a-seq) (take-monotonics > (rest a-seq)))
    :else (cons (first a-seq) (determine-monotonics (rest a-seq)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [mon (determine-monotonics a-seq)]
      (cons mon (split-into-monotonics (drop (count mon) a-seq))))))

(defn select [idx a-seq]
  (let [start (take idx a-seq)
        end (drop idx a-seq)]
    [(first end) (concat start (rest end))]))

(defn combine [init seq-seq]
  (map cons (repeat init) seq-seq))

(defn permutations [a-set]
  (cond 
    (empty? a-set) (cons '() '())
    (singleton? a-set) (cons a-set '()) 
    :else (let [a-seq (if (set? a-set) (seq a-set) a-set) 
                pairs (map select (range (count a-seq)) (repeat a-seq))]
            (mapcat (fn [pair] (combine (get pair 0) 
                                        (permutations (get pair 1)))) pairs))))

(defn sets-of-size [size a-seq]
  (if (== 1 size)
    (map (fn [x] #{x}) a-seq)
    (let [firsts (take (- (+ (count a-seq) 1) size) a-seq)
          ran (range 1 (inc (count firsts)))]
      (mapcat (fn [item x] (map cons 
                                (repeat item) 
                                (sets-of-size (dec size) (drop x a-seq)))) firsts ran))))
    

(defn powerset [a-set]
  (let [emp (cons #{} '())
        a-seq (seq a-set)]
    (if (empty? a-set)
      (set emp)
      (set (map set (concat emp (mapcat sets-of-size
                                        (range 1 (+ (count a-set) 1)) 
                                        (repeat a-seq))))))))

