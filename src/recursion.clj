(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(* 1 ( * 2 (* 4)))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (== 0 (count a-seq))
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    ()
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not= elem (first a-seq)) (sequence-contains? elem (rest a-seq))
   :else true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) 
   ()
   (pred? (first a-seq)) 
   (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else 
   ()))

(defn my-drop-while [pred? a-seq]
  (cond 
   (empty? a-seq)
   ()
   (pred? (first a-seq))
   (my-drop-while pred? (rest a-seq))
   :else
   a-seq))

(defn seq= [a-seq b-seq]
  (if (not= (count a-seq) (count b-seq))
    false
    (cond
     (and (empty? a-seq) (empty? b-seq)) 
     true
     (not= (first a-seq) (first b-seq))
     false
     :else
     (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else
   (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 0 how-many-times)
    ()
    (if (zero? how-many-times)
      ()
      (cons what-to-repeat 
            (my-repeat (dec how-many-times) what-to-repeat)))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    ()
    (if (= 1 up-to)
      '(0)
      (cons (dec up-to) (my-range (dec up-to))))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons (apply list a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [r-seq (reverse a-seq)]
    (map reverse (tails r-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list ())
    (distinct (map concat (tails a-seq) (reverse (inits a-seq))))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [next (first a-seq)]
      (let [new-freqs (if (= nil (get freqs next))
                        (assoc freqs next 1)
                        (assoc freqs next (inc (get freqs next))))]
        (my-frequencies-helper new-freqs
                               (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [[map-key map-val] (first a-map)]
      (concat (repeat map-val map-key) 
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (zero? n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-count (int (/ (count a-seq) 2))]
    [(my-take half-count a-seq) (my-drop half-count a-seq)]))


(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq)
   b-seq
   (empty? b-seq)
   a-seq
   (< (first a-seq) (first b-seq))
   (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else
   (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (seq-merge
     (merge-sort (first (halve a-seq)))
     (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

