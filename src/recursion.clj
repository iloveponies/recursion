(ns recursion)

(defn product [coll]
  
  (if (= 0 (count coll)) 
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (singleton? coll) (first coll)
   (empty? coll) nil
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
   []
   (concat
    (if (pred? (first a-seq)) [(first a-seq)] [])
    (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
 (if (empty? a-seq)
   false
   (or 
    (= elem (first a-seq))
    (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq)) 
      (concat [(first a-seq)] (my-take-while pred? (rest a-seq)))
      [])))

(defn my-drop-while [pred? a-seq]  
  (if (empty? a-seq) 
    []
    (if (pred? (first a-seq))
           (my-drop-while pred? (rest a-seq))
           a-seq)))


(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (concat [(f (first seq-1) (first seq-2))] (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
 (cond
  (= 0 n) 0
  (= 1 n) 1
  :else (+ (fib (- n 1)) (fib (- n 2)))) )


(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to)
    '()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list [])
    (cons a-seq (tails (rest a-seq)))))


(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (set (map concat (reverse (tails a-seq)) (inits a-seq))))


(defn my-frequencies-helper [freqs a-seq]
  (cond 
   (empty? a-seq) freqs
   (contains? freqs (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq)))) (rest a-seq) )
   :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) 
    []
    (let [k (first (keys a-map))
               n (get a-map k)]
           (concat (repeat n k) (un-frequencies (dissoc a-map k))))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    []
    (concat [(first coll)] (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [p (int (/ (count a-seq) 2))]
    [(my-take p a-seq) (my-drop p a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
     (and (= a nil) (not (= b nil))) b-seq
     (and (not (= a nil)) (= b nil)) a-seq
     (> a b) (concat [b] (seq-merge a-seq (rest b-seq)))
     (< a b) (concat [a] (seq-merge (rest a-seq) b-seq))
     :else [])))


(defn merge-sort [a-seq]
  (cond
   (= 0 (count a-seq)) []
   (= 1 (count a-seq)) [(first a-seq)]
   :else (let [[a b] (halve a-seq)]
           (seq-merge (merge-sort a) (merge-sort b)))))

(defn monotonic? [a-seq]
  (cond 
   (empty? a-seq) false
   (= 1 (count a-seq)) true
   :else (or (apply < a-seq) (apply > a-seq))))

(defn biggest-seq [a-seq]
  (if (= 0 (count a-seq))
    []
    (seq-max (first a-seq) (biggest-seq (rest a-seq)))))

(defn separate-monotonic [a-seq]
  (let [i (inits a-seq)
        m (filter monotonic? i)
        b (biggest-seq m)]
    (concat [b] [(drop (count b) a-seq)])))

(defn split-into-monotonics [a-seq]
  (if (= 0 (count a-seq))
    []
    (let [[a b] (separate-monotonic a-seq)]
      (cons a (split-into-monotonics b)))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

