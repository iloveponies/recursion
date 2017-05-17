(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [maxrest (max-element (rest a-seq))
          compar  (if (nil? maxrest) -1e6 maxrest)]
      (max (first a-seq) compar))))
    ;(let [r (rest a-seq)
    ;      mr (if (nil? r)
    ;           0
    ;           (max-element r))]
    ;  (max (first a-seq) mr))))
    ;(if (= a-seq nil)
    ;  0
    ;  (max (first a-seq) (max-element (rest a-seq))))))
;(defn max-element [a-seq]
;  (if (empty? a-seq)
;    nil ;a-seq
;    (max (first a-seq) (max-element (rest (a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [x (count seq-1)
        y (count seq-2)]
    (if (> x y) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil ;a-seq
    (let [f (first a-seq)
          fc (count f)
          r (longest-sequence (rest a-seq))
          rc (count r)]
      (if (> fc rc) f r))))
      
(defn my-map [f a-seq]
  (if (empty? a-seq)
    a-seq
    (cons (f (first a-seq))
          (my-map f (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)
          tail (my-filter pred? (rest a-seq))]
      (if (pred? f)
        (cons f tail)
        tail))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (== (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (pred? f)
        (cons f (my-take-while pred? r))
        '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (let [f (first a-seq)
          r (rest a-seq)]
      (if (pred? f)
        (my-drop-while pred? r)
        a-seq))))

(defn seq= [a-seq b-seq]
  (if (empty? a-seq)
    (empty? b-seq)
    (and
      (== (count a-seq) (count b-seq))
      (== (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (let [f1 (first seq-1)
          f2 (first seq-2)
          r1 (rest seq-1)
          r2 (rest seq-2)]
      (cons (f f1 f2) (my-map f r1 r2)))))

(defn power [n k]
  (if (zero? k) 1 (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1) n (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '() 
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) '() (cons (dec up-to) (my-range (dec up-to)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [f (first a-seq)
          newfreq (if (contains? freqs f)
                  (assoc freqs f (inc (get freqs f)))
                  (assoc freqs f 1))]
      (my-frequencies-helper newfreq (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (== 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (== 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [length (count a-seq)
        half (int (/ length 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (== 1 (count a-seq)))
    a-seq
    (let [[l r] (halve a-seq)]
      (seq-merge (merge-sort l) (merge-sort r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ^ DONE
; v TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO
(defn split-into-monotonics [a-seq]
  [:-])

; TODO
(defn permutations [a-set]
  [:-])

; TODO
(defn powerset [a-set]
  [:-])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ^ TODO
; v Abandoned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO
(defn tails [a-seq]
  (if (empty? a-seq)
    '[] 
    (cons (vec a-seq) (tails (rest a-seq)))))
  ;[:-])

; TODO
(defn inits [a-seq]
  [:-])

; TODO
(defn rotations [a-seq]
  [:-])

