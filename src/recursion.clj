(ns recursion)

(defn product [coll]
  (if (empty? coll)
  1
  (* (first coll)
   (product (rest coll)))))

(defn singleton? [coll]
 ; (let [fst (first coll) snd (not (empty? (rest coll)))]
   ;(and (and  (or (nil? fst) snd) (and (not (nil? fst)) (not snd)))
    ;   (and (not (or (nil? fst) snd)) (not (and (not (nil? fst)) (not snd)))))))
   ;   (or (or (not (or (nil? fst) snd)) (and (not (nil? fst)) (not snd)))))
  ; (and (not (nil? (first coll))) (empty? (rest coll))))
    (and (not (empty? coll)) (empty? (rest coll))))

   ;(str  (not (nil? (first coll))) (empty? (rest coll))))
   ;(str (count (first coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
  (first coll)
  (my-last (next coll)))
  )

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
  (first a-seq)
  (max (first a-seq) (max-element (rest a-seq)))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))


(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
  (first a-seq)
  (seq-max (first a-seq) (longest-sequence (rest a-seq)))))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
  a-seq
  (if (pred? (first a-seq))
  (cons (first a-seq) (my-filter pred? (rest a-seq)))
  (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)
    false
    (= (first a-seq) elem)
    true
    (not (= (first a-seq) elem))
    (sequence-contains? elem (rest a-seq))
    :else false))

(defn my-take-while [pred? a-seq]
  ;(cond
  ; (empty? a-seq) a-seq
   ;(pred? (first a-seq)) cons (first a-seq) (my-take-while pred? (rest a-seq))
   ;:else my-take-while pred? (rest a-seq)))
  (if (empty? a-seq)
  a-seq
  (if (pred? (first a-seq))
  (cons (first a-seq) (my-take-while pred? (rest a-seq)))
  (my-take-while pred? '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
  a-seq
  (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq
    ;(cons (first a-seq) (my-drop-while pred? (rest a-seq)))(not (and (empty? a-seq)) (empty? b-seq)))
    )))

(defn seq= [a-seq b-seq]
  (cond

     (and (empty? a-seq) (empty? b-seq))  false
      (and (and (empty? (rest a-seq)) (empty? (rest b-seq))) (not (nil? (first a-seq))) (= (first a-seq) (first b-seq))) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false ))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))
    ;(my-map f ((cons (f (first seq-1) (first seq-2)) rest seq-1)) (rest seq-2))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (or (zero? n)(= n 1))
    n
    (+ (fib (- n 1))  (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (or (zero? how-many-times) (> 0 how-many-times))
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '([])
     (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    '([])
    (cons  a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '()
    (conj a-seq (concat (rotations (cons (rest a-seq) (first a-seq))
                                      ;(rotations  (concat (rest a-seq) (rest (rest a-seq)))
                  ;(rest (concat (rest a-seq) (reverse (rest (reverse a-seq))) ))
                  )))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    (nil? (get freqs (first a-seq))) (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))
    :else (my-frequencies-helper (assoc freqs (first a-seq) (+ (get freqs (first a-seq)) 1)) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat (repeat (get (first a-map) 1) (get (first a-map) 0)) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
    (if (or (zero? n) (empty? coll))
   coll
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [leng (int (/ (count a-seq) 2))]
    (cons (my-take leng a-seq) (list (my-drop leng a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) '()
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq



     (and (< (first a-seq) (first b-seq)) (not (empty? (rest b-seq))) (> (first b-seq) (first (rest a-seq))))
        (concat (list (first a-seq)) (seq-merge (rest a-seq) b-seq))

   (and (< (first b-seq) (first a-seq)) (not (empty? (rest b-seq))) (> (first a-seq) (first (rest b-seq))))
         (concat (list (first b-seq)) (seq-merge a-seq (rest b-seq)))

       (< (first a-seq) (first b-seq))
        (concat (cons (first a-seq) (list (first b-seq))) (seq-merge (rest a-seq) (rest b-seq)))

    (< (first b-seq) (first a-seq))
        (concat (cons (first b-seq) (list (first a-seq))) (seq-merge (rest a-seq) (rest b-seq)))
           ))


;   _________
;   |        |
;   |        0
;   |       /|\
;   |       / \
;   |
;   |
(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (= (count a-seq) 1))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq) ))
    ))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

