(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

; (product [1 2 4])
; (* 1 (product [2 4]))
; (* 1 (* 2 (product [4])))
; (* 1 (* 2 (* 4 (product [])))))
; (* 1 (* 2 (* 4 1)))
; (* 1 (* 2 4))
; (* 1 8)
; 8

(defn singleton? [coll]
  (and (boolean (first coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn best-element [f a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    (first a-seq)
    (f (first a-seq) (best-element f (rest a-seq)))))

(defn max-element [a-seq]
  (best-element max a-seq))

(defn longer-sequence [seq-1 seq-2]
  (cond (empty? seq-1) second
        (empty? seq-2) first
        :else (longer-sequence (rest seq-1) (rest seq-2))))

(defn seq-max [seq-1 seq-2]
  ((longer-sequence seq-1 seq-2) [seq-1 seq-2]))

(defn longest-sequence [a-seq]
  (best-element seq-max a-seq))

(defn my-filter [pred? a-seq]
  (let [head (first a-seq)]
    (cond 
      (empty? a-seq) []
      (pred? head) (cons head (my-filter pred? (rest a-seq)))
      :else (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [head (first a-seq)]
    (cond 
      (empty? a-seq) []
      (not (pred? head)) []
      :else (cons head (my-take-while pred? (rest a-seq))))))

(defn my-drop-while [pred? a-seq]
  (let [head (first a-seq)]
    (cond
      (empty? a-seq) []
      (pred? head) (my-drop-while pred? (rest a-seq))
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) 
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0) 
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond 
    (= n 0) 0 
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (conj (tails (rest a-seq)) a-seq)))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [a-seq n]
  (if (= n 0)
    ()
    (cons a-seq 
      (rotations-helper 
        (cons (last a-seq) (butlast a-seq)) (- n 1)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotations-helper a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let [head (first a-seq)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper 
        (assoc freqs head (count (filter #(= head %) a-seq)))
        (remove #(= head %) a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]  
  (let [[value times] (first a-map)]
    (if (empty? a-map)
      ()
      (concat (repeat times value) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond 
    (< n 1) ()
    (empty? coll) ()
    :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (reverse (my-take (- (count coll) n) (reverse coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a-head (first a-seq)
        b-head (first b-seq)
        a-tail (rest a-seq)
        b-tail (rest b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< a-head b-head) (cons a-head (seq-merge a-tail b-seq))
      :else (cons b-head (seq-merge a-seq b-tail)))))

(defn merge-sort [a-seq]
  (let [[half1 half2] (halve a-seq)]
    (if (<= (count a-seq) 1)
      a-seq
      (seq-merge 
        (merge-sort half1)
        (merge-sort half2)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

