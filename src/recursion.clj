(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))))
  )

(defn singleton? [coll]
  (if (and (not (nil? (first coll))) (empty? (rest coll))) 
   true
    false
    ))

(defn my-last [coll]
 (cond
  (empty? coll) nil
   (singleton? coll) (first coll)
     :else
         (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
     (singleton? a-seq) (first a-seq)
      :else
       (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [seq1 (count seq-1)
        seq2 (count seq-2)] 
    (if(> seq1 seq2)
        seq-1
          seq-2
       ))
  )

(defn longest-sequence [a-seq]
   (if (empty? a-seq)
    nil
     (seq-max (first a-seq) (longest-sequence (rest a-seq)))
     ))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
     (if (pred? (first a-seq))
       (cons (first a-seq) (my-filter pred? (rest a-seq)))
         (my-filter pred? (rest a-seq))
       )
    ))

(defn sequence-contains? [elem a-seq]
   (cond
    (empty? a-seq)
       false
      (== (first a-seq) elem)
        true
         :else (sequence-contains? elem (rest a-seq))
     ))

(defn my-take-while [pred? a-seq]
   (cond
     (empty? a-seq)
        '()
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
          :else '()
    ))

(defn my-drop-while [pred? a-seq]
   (cond
     (empty? a-seq)
         '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
           :else a-seq
     ))

(defn seq= [a-seq b-seq]
   (cond 
     (and (empty? a-seq) (empty? b-seq))
        true
      (or (empty? a-seq) (empty? b-seq))
         false
         :else (recur (rest a-seq) (rest b-seq))

      ))

(defn my-map [f seq-1 seq-2]
   (cond
     (or (empty? seq-1) (empty? seq-2)) 
        '()
       :else
        (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))
      ))

(defn fib [n]
  (cond
    (== n 0) 0
      (== n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
   ))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0)
      '()
      :else
       (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))

   ))

(defn my-range [up-to]
  (cond
    (= up-to 0)
      '()
      :else
       (cons (- up-to 1) (my-range (- up-to 1)))
   ))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
       (list a-seq)
       :else
         (cons a-seq (tails (rest a-seq)))
   ))

(defn inits [a-seq]
  (cond
    (empty? a-seq)
       (list a-seq)
      :else
       (cons a-seq (inits (reverse (rest (reverse a-seq)))))
   ))

(defn rotations [a-seq]
 (cond
   (empty? a-seq)
      (list a-seq)
      :else
        (rest (map concat (reverse (tails a-seq)) (inits a-seq)))
   ))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
      :else
       (my-frequencies-helper
         (if (contains? freqs (first a-seq))
             (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
             (assoc freqs (first a-seq) 1)
         )
      (rest a-seq))

   ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) a-map
      :else
       (let [[flag times] (first a-map)]
         (concat (repeat times flag) (un-frequencies (rest a-map))))
   ))

(defn my-take [n coll]
  (cond
    (empty? coll) '()
     (= n 0) '()
       (>= n (count coll)) coll
     :else
        (cons (first coll) (my-take (dec n) (rest coll)))
    ))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
      (= n 0) coll
        (>= n (count coll)) '()
      :else 
       (cons (get coll n) (my-drop (inc n) coll))
   ))

(defn halve [a-seq]
  (cond
    (empty? a-seq) '()
    :else
        (let [divide (int (/ (count a-seq) 2))]
     (vector (my-take divide a-seq) (my-drop divide a-seq))
    )
   ))

(defn seq-merge [a-seq b-seq]
  (
   let [new-seq (fn [new-seq a-seq b-seq]
   (cond
     (and (empty? a-seq) (empty? b-seq)) '()
      (empty? a-seq) (concat new-seq b-seq)
        (empty? b-seq) (concat new-seq a-seq)
      :else
        (let [elema (first a-seq) elemb (first b-seq)] (
          if (<= elema elemb)
            (recur (concat new-seq [elema]) (rest a-seq) b-seq)
            (recur (concat new-seq [elemb]) a-seq (rest b-seq))

         ))
    )
                  )]
    (new-seq '() a-seq b-seq)
   ))

(defn merge-sort [a-seq]
   (cond
     (empty? a-seq) '()
       (= (count a-seq) 1) a-seq
      :else 
        (let [[left right] (halve a-seq)]
               (seq-merge (merge-sort left) (merge-sort right)))
    ))

(defn split-into-monotonics [a-seq]
  (cond 
    (empty? a-seq) '()
      :else ()
   ))

(defn permutations [a-set]
  (cond
    (empty? a-set) (cons '() '())
      (= (count a-set) 1) (list a-set)
       :else ()
   ))

(defn powerset [a-set]
  (cond
    (empty? a-set) (hash-set (hash-set))
     :else
       (clojure.set/union (powerset (next a-set)) (map #(conj % (first a-set)) 
           (powerset (next a-set))))
   ))