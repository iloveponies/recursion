(ns recursion)

(defn product 
  "Product by recursion for a collection."
  [coll]
  (if (empty? coll)
      1
      (* (first coll)
         (product (rest coll)))))

(defn singleton?
  "Checks if a collection has exactly one member."
  [coll]
  (if (not (empty? coll))
      (empty? (rest coll))
      false))

(defn my-last 
  "Returns the last member in a collection."
  [coll]
  (if (or (empty? coll) (singleton? coll))
      (first coll)
      (my-last (rest coll))))

(defn max-element 
  "Finds the maximum element in a sequence."
  [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) 
           (max-element (rest a-seq))))))

(defn seq-max
  "Returns the longer sequence of the two. (Can I use count already?)" 
  [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
      seq-1
      seq-2))   

(defn longest-sequence 
  "Returns the longest sequence in a sequence of sequences."
  [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
          (first a-seq)
          (seq-max (first a-seq)
                   (longest-sequence (rest a-seq)))))

(defn my-filter 
  "Filter function implementation."
  [pred? a-seq]
  (if (empty? a-seq)
      a-seq
      (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? 
  "Determine if an element is found in a sequence."
  [elem a-seq]
  (if (empty? a-seq)
      false
      (if (= elem 
             (first a-seq))
          true
          (sequence-contains? elem (rest a-seq)))))

(defn my-take-while
  "Returns a-seq elements while pred? is true." 
  [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))


(defn my-drop-while 
  "Drops elements from a sequence until pred? is false."
  [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
       a-seq
       (my-drop-while pred? (rest a-seq))))

(defn seq= 
  "Compares if two sequences are equal."
  [a-seq b-seq]
  (if (and (empty? a-seq) 
           (empty? b-seq))
      true
      (if (= (first a-seq) 
             (first b-seq))
          (seq= (rest a-seq) (rest b-seq))
          false)))

(defn my-map 
  "Map implementation with recursion."
  [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power 
  "Calculates n^k. Inefficient, naive implementation for the sake of learning."
  [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k)))))

(defn fib 
  "Computes nth fibonacci number."
  [n]
  (if (= 0 n)
    0
    (if (= 1 n)
      1
      (+ (fib (dec n))
         (fib (dec (dec n)))))))

(defn my-repeat 
  "Repeats what-to-repeat how-many-times times."
  [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range 
  "Returns a sequence that counts down to 0 from (up-to - 1)."
  [up-to]
  (if (<= up-to 0)
    '()
    (cons (dec up-to) 
          (my-range (dec up-to)))))

(defn tails
  "Returns all suffixes of a sequence." 
  [a-seq]
  (if (empty? a-seq)
    '(())    
    (cons a-seq (tails (rest a-seq)))))

(defn inits 
  "Returns all prefixes of a sequence." 
  [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))

(defn rot1 
  "Rotates a sequence by one. (For (1 2 3 4 5) returns (2 3 4 5 1))"
  [a-seq]
  (concat (rest a-seq) 
        (cons (first a-seq) nil)))

(defn rotations-helper
  "Counter-helper function for rotations."
  [n a-seq]
  (if (<= n 0)
    '()
    (cons a-seq 
          (rotations-helper (dec n) (rot1 a-seq)))))

(defn rotations  
  "Returns a collection of all rotations for a sequence."
  [a-seq] 
  (if (empty? a-seq)
    '(())
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper 
  "Calculate frequencies in a sequence: 
   - Assoc's the first element and its frequency
   - Drops all elements that equal the first and recurses."
  [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs 
                                  (first a-seq)
                                  (count-elem (first a-seq) a-seq))
                           (filter (fn [x] (not (= x (first a-seq))))
                                   a-seq))))

(defn my-frequencies 
  "Calculate frequencies in a sequence using a helper function."
  [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies 
  "Takes a map with values and frequencies, and produces a sequence."
  [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (first (vals a-map))
                    (first (keys a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take 
  "Returns n first items from a collection."
  [n coll]
  (if (or (<= n 0)
          (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n)
                   (rest coll)))))

(defn my-drop 
  "Drops n first items from a collection."
  [n coll]
  (if (or (<= n 0)
          (empty? coll))
    coll
    (my-drop (dec n)
             (rest coll))))

(defn halve 
  "Splits a sequence in half."
  [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))]
    (cons (my-take midpoint a-seq)
          (cons (my-drop midpoint a-seq) nil))))

(defn seq-merge-helper 
  "Merges two ordered sequences."
  [result a-seq b-seq]
  (cond 
    (and (empty? a-seq)
         (empty? b-seq)) result
    (empty? a-seq) (concat result b-seq)
    (empty? b-seq) (concat result a-seq)
    :else (if (< (first a-seq) 
                 (first b-seq))
            (seq-merge-helper (concat result (cons (first a-seq) nil))
                              (rest a-seq) 
                              b-seq)
            (seq-merge-helper (concat result (cons (first b-seq) nil))
                              a-seq 
                              (rest b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))

(defn merge-sort 
  "Merge-sorts a sequence."
  [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (seq-merge (merge-sort (first (halve a-seq)))
               (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
