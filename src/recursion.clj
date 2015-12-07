(ns recursion)

(defn product [coll]
   (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll))))
  )

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll)))
  )

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll)) (first coll) (my-last (rest coll)))
  )

(defn max-element [a-seq]
  (if (empty? a-seq) nil (apply max a-seq))
  )

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq) (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq)))
  )))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    ; added only if prediction holds
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )
  )
  )

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq)))
)

(defn my-take-while [pred? a-seq]
;;   (longest-sequence (my-filter pred? a-seq))
  (if (empty? a-seq) '()
    (if (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq))
                            ) '()))

  )

(defn my-drop-while [pred? a-seq]
 (if (empty? a-seq) '()
   (if (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
              a-seq
  )))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (not (= (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))
  ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)) )
  ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))
))

; animace pro (power 2 4)
; * 2 (power 2 3)
; * 2 ( * 2 (power 2 2))
; * 2 ( * 2 (* 2 (power 2 1)))
; * 2 ( * 2 (* 2 (* 2 (power 2 0))))
; * 2 ( * 2 (* 2 (* 2 1))
; * 2 ( * 2 (* 2 2)
; * 2 ( * 2 4)
; * 2 8
; 16


(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))
))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)))
)

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))
))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (tails (rest a-seq))))
)

(defn inits [a-seq]
    (reverse (map reverse (tails (reverse a-seq))))
)

(defn rotations [a-seq]
  (if (empty? a-seq)
    [()]
    ; udelaji se z toho dvojice v prohozenem poradi spravne
    (rest (map concat (tails a-seq) (inits a-seq)))
))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [my-key (first a-seq)
          new-set (if (contains? freqs my-key)
                     (assoc freqs my-key (inc (get freqs my-key)))
                     (assoc freqs my-key 1))
          ]
      (my-frequencies-helper new-set (rest a-seq))))
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq)
)

(defn un-frequencies [a-map]
  (let [what (keys a-map) many (vals a-map)]
;;     (my-repeat (first many) (first what))
    (apply concat (map my-repeat many what))
    )
)

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))
))

(defn my-drop [n coll]
  (if (and (< n 1) (not (empty? coll)))
    (cons (first coll) (my-take (dec n) (rest coll)))
    (if (empty? coll)
      '()
      (my-drop (dec n) (rest coll))
      ))
)

(defn halve [a-seq]
  (let [first-len (int (/ (count a-seq) 2))
        second-len (- (count a-seq) first-len)
        first-half (my-take first-len a-seq)
        second-half (my-drop first-len a-seq)
        ]
    [first-half second-half]
))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? b-seq) a-seq
   (empty? a-seq) b-seq
   :else (if (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
      (cons (first b-seq) (seq-merge a-seq (rest b-seq))))
    )
  )

(defn merge-sort [a-seq]
  (cond
   (= 0 (count a-seq)) '()
   (= 1 (count a-seq)) a-seq
   ; seq-merge first-half second-half
   ; map sort to both halves since it takes one param
   :else (apply seq-merge (map merge-sort (halve a-seq)))
))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])
