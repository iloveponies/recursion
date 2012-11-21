(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

;
; Exercise 2
; ==========
;
;   (product [1 2 4])
; = (product (cons 1 (cons 2 (cons 4 '())))))
;=> (* 1 (product (cons 2 (cons 4 '())))))
;=> (* 1 (* 2 (product (cons 4 '()))))
;=> (* 1 (* 2 (* 4 (product '()))))
;=> (* 1 (* 2 (* 4 1))))
;=> (* 1 (* 2 4))
;=> (* 1 8)
;=> 8
;

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn my-last [coll]
  (if (= '() (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max-element (cons (max (first a-seq) (first (rest a-seq)))
                         (rest (rest a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (cond
    (empty? seq-1)
      seq-2
    (empty? seq-2)
      seq-1
    :else (if (> (count seq-1) (count seq-2))
            seq-1
            seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (longest-sequence (cons (seq-max (first a-seq) (first (rest a-seq)))
                              (rest (rest a-seq)))))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
    :else
      (my-filter pred? (rest a-seq))))

(defn sequence-contains? 
  [elem a-seq]
  (cond
    (empty? a-seq)
      false
    (= elem (first a-seq))
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (not (pred? (first a-seq)))
      ()
    :else
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))))

(defn my-drop-while 
  [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (not (= (first a-seq) (first b-seq)))
      false
    :else
      (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1)
      seq-1
    (empty? seq-2)
      seq-2
    :else
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (== 0 n)
      0
    (== 0 k)
      1
    :else
      (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0)
      0
    (== n 1)
      1
    :else
      (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    ()
    (cons (dec up-to) 
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (list ())
    (cons a-seq (inits (pop a-seq)))))

(defn rotations [a-seq]
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [hash-key (first a-seq)]
      (if (contains? freqs hash-key)
        (my-frequencies-helper
          (conj freqs [hash-key (inc (freqs hash-key))])
          (rest a-seq))
        (my-frequencies-helper
          (conj freqs [hash-key 1])
          (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [keyseq a-map result]
  (if (empty? keyseq)
    result
    (concat 
      (repeat (a-map (first keyseq)) (first keyseq))
      (un-frequencies-helper (rest keyseq) a-map result))))

(defn un-frequencies [a-map]
  (un-frequencies-helper (keys a-map) a-map []))

(defn my-take [n coll]
  (cond
    (empty? coll)
      () 
    (> n 0)
      (cons (first coll) (my-take (dec n) (rest coll)))
    :else 
      ()))

(defn my-drop [n coll]
  (cond
    (empty? coll)
      ()
    (> n 0)
      (my-drop (dec n) (rest coll))
    :else
      coll))

(defn halve [a-seq]
  (let [length (count a-seq)
        half   (int (/ length 2))]
    (cons (take half a-seq) (list (drop half a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    :else
      (if (< (first a-seq) (first b-seq))
        (cons 
          (first a-seq) 
          (seq-merge (rest a-seq) b-seq))
        (cons
          (first b-seq)
          (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (if (>= 1 (count a-seq))
    a-seq
    (let [fir (first (halve a-seq))
          sec (first (rest (halve a-seq)))]
      (seq-merge 
        (merge-sort fir) 
        (merge-sort sec)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

