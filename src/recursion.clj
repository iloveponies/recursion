(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))
; if we could assume coll does not contain nil,
; we could simply check if there is a second element
; instead of using the rest-function.

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
    (if (> len1 len2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [elem (first a-seq)]
      (if (pred? elem)
        (cons elem
              (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))
          

(defn sequence-contains? [elem a-seq]
  (cond 
    (empty? a-seq) false
    (== elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) 
                                (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (== (first a-seq) (first b-seq)) (seq= (rest a-seq)
                                           (rest b-seq))
    :else false))

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
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(()) 
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

; We reverse to remove the last element, reverse again to restore
; the original order. The return value should be reversed to get the
; inits sorted from shortest to longest sequences.


; Sometimes it is best to think outside of the box.
; I took the freedom to create a helper function to rotations
; "Make things as simple as possible, but not simpler." - Einstein
(defn rotate [a-seq times]
  (if (zero? times)
    '()
    (cons a-seq
          (rotate (concat (rest a-seq)
                          (vector (first a-seq))) (dec times)))))
  
(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotate a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          new-count (if (contains? freqs elem)
                      (assoc freqs elem (inc (get freqs elem)))
                      (assoc freqs elem 1))]
      (my-frequencies-helper new-count (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [elem (first a-map)]
      (concat (repeat (last elem) (first elem))
                     (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))
        a (my-take mid a-seq)
        b (my-drop mid a-seq)]
    [a b]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq)
       (first b-seq)) (cons (first a-seq)
                            (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq)
                (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '() 
    (let [monotonic? (fn [s] (if (empty? s)
                               false
                               (or (apply <= s) (apply >= s))))
          mono (last (take-while
                     monotonic? (drop 2 (reverse (inits a-seq)))))]
      (if (or (empty? mono) (nil? mono))
        a-seq
        (cons mono
              (split-into-monotonics
                (drop (count mono) a-seq)))))))

; More helpers...
(defn permutations-helper [i goal current-coll]
  (let [n1 (get current-coll i)
        n2 (get current-coll (inc i))
        swapped (assoc (assoc current-coll i n2) (inc i) n1)
        next-i (mod (inc i) (dec (count goal)))]
    (if (= goal swapped)
      (cons current-coll '())
      (cons current-coll (permutations-helper next-i goal swapped)))))

(defn permutations [a-set]
  (cond 
    (empty? a-set) '(())
    (singleton? a-set) (cons (seq a-set) '())
    :else (permutations-helper
            0 (apply vector a-set) (apply vector a-set))))

; Powerset
; if too complex then split it into parts
(defn pset-pairs [a-set]
  (if (empty? a-set)
    a-set
    (concat (pset-pairs (rest a-set))
            (map (fn [n] (set (vector (first a-set) n))) (rest a-set)))))

(defn ps-combine [singles groups]
  (if (empty? groups)
    '()
  (let [filtered (filter
                   (fn [c] ((complement contains?) c (first singles))) groups)
        combined (map (fn [c] (conj c (first singles))) filtered)]
    (concat combined (ps-combine (rest singles) filtered)))))

(defn ps-combinations [singles groups len tlen]
  (if (== len tlen)
    #{}
    (let [new-groups (ps-combine singles
                        (filter (fn [c] (== (count c) len)) groups))]
    (concat new-groups
       (ps-combinations singles new-groups (inc len) tlen)))))
  
(defn powerset [a-set]
  (if (or (empty? a-set) (singleton? a-set))
    (conj #{} a-set)
    (let [len (count a-set)
          singles (map set (map vector a-set)) 
          pairs (pset-pairs a-set)
          more (if (< len 2)
                 '()
                 (ps-combinations a-set pairs 2 len))]
      (concat [#{}] singles pairs more))))
