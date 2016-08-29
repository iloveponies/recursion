(ns recursion)

(defn sum [coll]
  (if (empty? coll)
    0
    (+ (first coll)
       (sum (rest coll)))))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
  )
)

(defn singleton? [coll]
  (and (not (empty? coll)) ; coll should not be empty, i.e. not []
       (empty? (rest coll))) ; the rest of coll should be empty, i.e. []
)

(defn my-last [coll]
  (if (singleton? coll)
    (first coll)
    (if (empty? coll)
      nil
      (my-last (rest coll))
    )
  )
)


(defn max-element [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (max (first a-seq) (max-element (rest a-seq)))
    )
  )
)

(defn min-element [a-seq]
  (if (singleton? a-seq)
    (first a-seq)
    (if (empty? a-seq)
      nil
      (min (first a-seq) (min-element (rest a-seq)))
    )
  )
)

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2
  )  
)

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
  )
)

(defn my-map [f a-seq]
  (if (empty? a-seq)
    a-seq
    (cons (f (first a-seq)) (my-map f (rest a-seq)))
  )
)

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )
  )
)

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (== elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq)) 
  )
)

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq                        
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     []
  )
)

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     a-seq                        
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   :else
     a-seq
  )
)

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (== (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else 
      false
  )
)

(defn my-map [f seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2))
      []
    (or (empty? seq-1) (empty? seq-2))
      []
    :else 
      (cons (f (first seq-1) (first seq-2)) 
            (my-map f (rest seq-1) (rest seq-2))
      )
  )
)

(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

; (indexed [:a :b :c]) ;=> ([0 :a] [1 :b] [2 :c])

(defn power [n k]
  (if (>= 0 k)
    1
    (* n (power n (dec k)))
  )
)

(defn fib [n]
  (if (== n 0)
    0
    (if (== n 1)
      1
      (+ (fib (- n 1)) 
         (fib (- n 2)))
    )
  )
)

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
  )
)

(defn my-range [up-to]
  (if (>= 0 up-to)
    []
    (cons (- up-to 1) (my-range (dec up-to)))
  )
)

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq [])
    (cons a-seq (tails (rest a-seq)))
  )
)

; (tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
; (inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))
; You can output the tails and inits in any order you like.
; (inits [1 2 3 4]) ;=> ((1 2) () (1 2 3) (1) (1 2 3 4))

(defn all-but-last [a-seq]
  (if (singleton? a-seq)
    '()
    (cons (first a-seq) (all-but-last (rest a-seq)))
  )
)
; (all-but-last [1 2])
; (all-but-last [1])

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons a-seq [])
    (cons a-seq (inits (all-but-last a-seq)))
  )
)
; (inits [1 2 3 4])

(defn rotate [a-seq]
  (cond
    (empty? a-seq)
      (cons a-seq [])
    :else 
      (concat (rest a-seq) (cons (first a-seq) '()))
  )
)

(defn rotations-helper [n a-seq]
  (cond
    (empty? a-seq)
      (cons a-seq [])
    (== n 1)
      (cons a-seq [])
    :else
      (cons a-seq (rotations-helper (dec n) (rotate a-seq)))
  )
)

(defn rotations [a-seq]
  (rotations-helper (count a-seq) a-seq)
)

; (def t1 {:a 3, :b 4, "moi" 5})
; (contains? t1 "moi") or (nil? (get t1 "moi1234"))
; (assoc t1 "moi" 666)
; (assoc t1 "moi23" 666)
; (get t1 "moi")


(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq)
      freqs
    (contains? freqs (first a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq)))) (rest a-seq))
    :else
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
  )
)

; (if (contains? freqs (first a-seq))
;         (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
;         (assoc freqs (first a-seq) 1)
;       )
;       (my-frequencies-helper freqs (rest a-seq))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequency [a-map]
  (cond
    (empty? a-map)
    []
  :else
    (vec (repeat (first (rest (first a-map))) (first (first a-map))))
  )
)

(defn un-frequencies [a-map]
  (cond
    (empty? a-map)
      []
    :else
      (concat (un-frequency a-map) (un-frequencies (rest a-map)))
  )
)

(defn my-take [n coll]
  (cond
    (empty? coll)
      coll
    (== n 0)
      '()
    (== n 1)
      (cons (first coll) [])
    :else
      (cons (first coll) (my-take (dec n) (rest coll)) )
  )
)

(defn my-drop [n coll]
  (cond
    (< (count coll) n)
    '()
    (empty? coll)
      coll
    (== n 1)
      (cons (get coll (- (count coll) n)) [])
    :else
      (cons (get coll (- (count coll) n)) (my-drop (dec n) coll) )
  )
)

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (- (count a-seq) (int (/ (count a-seq) 2))) a-seq)]
)

(defn find-min [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      nil
    :else
      (min (min-element a-seq) (min-element b-seq))
  )
)

(defn add-sequence [k a-seq]
  (concat [k] a-seq)
)

(defn willsorted? [a-seq]
  ; (if (or (apply <= a-seq) (apply >= a-seq))
  (if (apply <= a-seq)
    true
    false
  )
)

; (defn willsort [acoll]
;   (cond
;     (singleton? acoll)
;       (concat acoll [])
;     (willsorted? acoll)
;       (concat acoll [])
;     ; (== 2 (count acoll))
;     ;   (concat [(min (get (vec acoll) 0) (get (vec acoll) 1))] [(max (get (vec acoll) 0) (get (vec acoll) 1))])
;     :else
;       (willsort (willmerge (get (halve (vec acoll)) 0) (get (halve (vec acoll)) 1)))
;   )
; )

; (defn willmerge [bcoll ccoll]
; (cond
;     (and (empty? bcoll) (empty? ccoll))
;       '()
;     (and (singleton? bcoll) (singleton? ccoll))  
;       (concat [(min (first bcoll) (first ccoll))] [(max (first bcoll) (first ccoll))])
;     :else
;       (concat (willsort bcoll) (willsort ccoll))
;   )
; )

(defn willtest [bcoll ccoll]
  [bcoll ccoll]
)

(defn seq-merge [b-seq c-seq]
  (cond 
    (nil? b-seq) 
      c-seq
    (nil? c-seq) 
      b-seq
    :else 
      (let [[l & *c-seq] c-seq
            [r & *b-seq] b-seq]
        (if (<= l r) 
          (cons l (seq-merge *c-seq b-seq))
          (cons r (seq-merge c-seq *b-seq))
        )
      )
  )
)

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[left right] (split-at (/ (count a-seq) 2) a-seq)]
      (seq-merge (merge-sort left) (merge-sort right)))
  )
)

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false
  )
)

(defn inits2 [a-seq]
  (reverse (inits a-seq))
)

(defn monotonic-seq [n a-seq]
  (cond 
    (singleton? a-seq) 
      a-seq
    (empty? a-seq)
      a-seq
    :else
      (if (monotonic? (take n a-seq))
        (take n a-seq)
        (monotonic-seq (dec n) a-seq)
      )
  )
)

; (drop (count (monotonic-seq (count a-seq) a-seq)) a-seq)
; (cons a-seq (split-into-monotonics (last a-seq)))

(defn split-into-monotonics [a-seq]
  (cond 
    (empty? a-seq)
      a-seq
    :else
      (cons (monotonic-seq (count a-seq) a-seq) 
            (split-into-monotonics (last (split-at (count (monotonic-seq (count a-seq) a-seq)) a-seq)))
      )
  )
)

(defn permutations [a-set]
  (if (empty? a-set)
    (list ())
    (apply concat (map (fn [x] (map cons (repeat (first x)) (permutations (rest x)))) (rotations a-set)))
  )
)

(defn powerset [a-set]
  (if (empty? a-set)
    (list ())
    (clojure.set/union (powerset (rest a-set)) (map (fn [x] (conj x (first a-set))) (powerset (rest a-set))))
  )
)