(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

; (product [1 2 4]
; (product (cons 1 (cons 2 (cons 4 []))))
;=> (* 1 (product (cons 2 (cons 4 []))))
;=> (* 1 (* 2 (product (cons 4 []))))
;=> (* 1 (* 2 (* 4 (product []))))
;=> (* 1 (* 2 (* 4 1)))
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

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
  (cond
    (empty? seq-1) seq-2
    (empty? seq-2) seq-1
    (= (rest seq-2) (seq-max (rest seq-1) (rest seq-2))) seq-2
    :else seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
   (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first-elem (first a-seq) others (rest a-seq)]
      (if (pred? first-elem)
        (cons first-elem (my-filter pred? others))
        (my-filter pred? others)))))
    
(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    '()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))


(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq) (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f a-seq b-seq]
  (if
    (or (empty? a-seq) (empty? b-seq))
    '()
    (cons (f (first a-seq) (first b-seq)) 
          (my-map f (rest a-seq) (rest b-seq)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if
    (< how-many-times 1)
    ()
    (cons what-to-repeat 
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if
    (zero? up-to)
    ()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
    (map reverse (tails (reverse a-seq))))

(defn n-right-rotations [a-seq n]
  (let [next-rotation (concat (rest a-seq) (cons (first a-seq) '()))]
    (if(< n 1) 
      '()
      (cons a-seq (n-right-rotations next-rotation (dec n))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (n-right-rotations a-seq (count a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-elem (first a-seq)
          first-elem-freq (if (freqs first-elem)
                          (freqs first-elem)
                          0)]
      (my-frequencies-helper 
        (assoc freqs first-elem (+ first-elem-freq 1))
        (rest a-seq)))))
        
(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [first-elem (first a-map)]
      (concat 
        (repeat (val first-elem) (key first-elem))
        (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '() 
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [size (count a-seq)
        half-size (int (/ size 2))]
    (vector (my-take half-size a-seq)
            (my-drop half-size a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [a-first (first a-seq) b-first (first b-seq)]
            (if (< a-first b-first)
              (cons a-first (seq-merge (rest a-seq) b-seq))
              (cons b-first (seq-merge (rest b-seq) a-seq))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a-half b-half] (halve a-seq)
          sorted-a-half (merge-sort a-half)
          sorted-b-half (merge-sort b-half)]
      (seq-merge sorted-a-half sorted-b-half))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic-inits (filter monotonic? (inits a-seq))
          max-monotonic-init-length (apply max (map count monotonic-inits))
          longest-monotonic-init (take max-monotonic-init-length a-seq)
          tail (drop max-monotonic-init-length a-seq)]
      (cons longest-monotonic-init (split-into-monotonics tail)))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

