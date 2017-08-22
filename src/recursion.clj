(ns recursion)
(defn product [coll]
  (if 
      (empty? coll)
    1
    (* 
     (first coll)
     (product (rest coll)))))

(defn singleton? [coll]
  (if 
      (empty? coll) 
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if 
      (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) 
          (empty? a-seq))

    (my-last a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let 
      [count-1 (count seq-1)
       count-2 (count seq-2)]
    (if 
        (> 
         count-1 
         count-2)
      seq-1
      seq-2)))


(defn seq= [a-seq b-seq]
  (cond
   (not= 
    (count a-seq) 
    (count b-seq))
   false
   
   (and
    (empty? a-seq)
    (empty? b-seq)) 
   true

   (= 
    (first a-seq) 
    (first b-seq))
   (seq= 
    (rest a-seq) 
    (rest b-seq))

   :else false))

(defn my-map [f seq-1 seq-2]
  (if 
      (or 
       (empty? seq-1) 
       (empty? seq-2))
    '()
    (cons 
     (f 
      (first seq-1) 
      (first seq-2))
     (my-map 
      f 
      (rest seq-1) 
      (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= n 1) 1
   :else
   (+ (fib (- n 1))
      (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (or 
       (zero? how-many-times)
       (neg? how-many-times))
    '()
    (cons
     what-to-repeat
     (my-repeat
      (dec how-many-times)
      what-to-repeat))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()                          ;; limiting condition
    (if (= 
         (pred? (first a-seq))   ;; if pred returns true, put it in list
         true)
      (cons (first a-seq)
            (my-filter pred? 
                       (rest a-seq)))
      (my-filter pred?           ;; else skip to next element
                 (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) 
   false

   (not= 
    elem 
    (first a-seq))
   (sequence-contains? 
    elem 
    (rest a-seq))

   :else
   true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
   '()

   (= (pred? (first a-seq)) 
      true)
   (cons 
    (first a-seq) 
    (my-take-while 
     pred? 
     (rest a-seq)))

   :else
   '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
   '()
   
   (= 
    (pred? (first a-seq)) 
    true)
   (my-drop-while 
    odd? 
    (rest a-seq))
   
   :else
   a-seq))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse
                (tails 
                 (reverse a-seq)))))

(defn rotations
  ([a-seq] 
   (rotations a-seq 
              (count a-seq)))

  ([a-seq count] 
   (let [new-seq 
         (concat (rest a-seq) 
                 (cons 
                  (first a-seq) 
                  '()))]
     (cond 
      (empty? a-seq) '(())
      (= count 0) ()
      :else
      (cons new-seq
            (rotations new-seq (dec count)))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key (first a-seq)
          new-freq (if (contains? freqs key)
                     (assoc freqs 
                       key 
                       (inc (get freqs key)))
                     
                     (assoc freqs key 1))]
      (my-frequencies-helper new-freq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [key (first (first a-map))
          value (second (first a-map))] 
        (concat (repeat value key) 
                (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    ()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [size (count a-seq)
        middle (int (/ size 2))]
    (if (= size 0)
      []
      (conj [] 
            (my-take middle a-seq) 
            (my-drop middle a-seq)))))

(defn seq-merge-helper [final-seq a-seq b-seq]
  (cond
   (empty? a-seq) (concat final-seq b-seq)
   (empty? b-seq) (concat final-seq a-seq)

   (< (first a-seq) (first b-seq))
   (concat
    final-seq
    (cons (first a-seq) '())
    (seq-merge-helper final-seq 
                      (rest a-seq) 
                      b-seq))

   :else (concat
          final-seq
          (cons (first b-seq) '())
          (seq-merge-helper final-seq
                            a-seq
                            (rest b-seq)))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper [] a-seq b-seq))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (seq-merge (merge-sort 
                (first (halve a-seq)))
               (merge-sort 
                (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

