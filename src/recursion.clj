(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (let [ f (first coll)
         r (rest coll)]
    (and (not (empty? coll)) 
         (not (= f false))
         (empty? r))))

(defn my-last [coll]
  (let [f (first coll)
        r (rest coll)]
    (if (empty? r)
        f
        (my-last r))))

(defn max-element [a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
  (cond
    (empty? a-seq) nil
    (empty? r) f
    :else (max f (max-element r)))))

(defn seq-max [seq-1 seq-2]
  (let [s1 (count seq-1)
        s2 (count seq-2)]
    (if (> s1 s2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
  (cond
    (empty? a-seq) nil
    (empty? r) f
    :else (seq-max f (longest-sequence r)))))

(defn my-filter [pred? a-seq]
  (let [frst (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (not (pred? frst)) (my-filter pred? (rest a-seq))
      :else (cons frst (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else (my-take-while pred? '())))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq 
    (my-drop-while pred? (rest a-seq))))


(defn seq= [a-seq b-seq]
  (cond
    (or (and (not (empty? a-seq)) (empty? b-seq))
        (and (empty? a-seq) (not (empty? b-seq)))) false
    (and (empty? a-seq) (empty? a-seq)) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)) 
    :else false
    ))

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
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))    

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '() 
    ()))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [key (first a-seq)
          new-map (if (contains? freqs key)
                    (assoc freqs key (inc (freqs key)))
                    (assoc freqs key 1))]
      (my-frequencies-helper new-map (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (or (empty? coll)
          (= 0 n))
    nil 
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop-helper [seq n coll]
  (if (empty? coll)
    seq
    (let [seq (if (< n (count coll))
                (cons (first coll) seq)
                seq)]
     (my-drop-helper seq n (rest coll)))))

(defn my-drop [n coll]
    (my-drop-helper '() n (reverse coll)))

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

