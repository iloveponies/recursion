(ns recursion)

(defn product [coll]
  (if (empty? coll)
  1
  (* (first coll) (product (rest coll)))))
; product '(1 2 4)
; = product (*1 (*2 (*4 (product '()))))
; = 1*2*4*1 = 8

(defn singleton? [coll]
  (if (empty? coll) 
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (my-last (sort (seq (set a-seq)))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (if (singleton? (rest a-seq))
        (seq-max (first a-seq) (first (rest a-seq)))
        (seq-max (first a-seq) (longest-sequence (rest a-seq)))))))

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
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq))
      (if (singleton? a-seq)
        (first a-seq)
        (cons (first a-seq) (my-take-while pred? (rest a-seq))))
      [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    []
    (if (pred? (first a-seq))
      (if (singleton? a-seq)
        ([])
        (my-drop-while pred? (rest a-seq)))
      a-seq)))


(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))
  
(defn power [n k]
  (if (= k 0)
    1
    (if (= n 0)
      0
      (* n (power n (dec k))))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (if (= how-many-times 1)
      [what-to-repeat]
      (conj (my-repeat (dec how-many-times) what-to-repeat) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= 0 up-to) []
    (= 1 up-to) [0]
    :else (apply conj [(dec up-to)] (my-range (dec up-to)))
  ))

; (defn fizzbuzz [n]
;   (cond
;     (divides? 15 n) "gotcha!"
;     (divides? 5 n) "buzz"
;     (divides? 3 n) "fizz"
;     :else "")
; )

(defn tails [a-seq]
  (cond
    (empty? a-seq) [[]]
    (singleton? a-seq) [a-seq []]
    :else (concat [a-seq] (tails (rest a-seq)))))
;    :else (concat [a-seq] (first (tails (rest a-seq))))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond
    (empty? a-seq) [[]]
    (singleton? a-seq) [a-seq]
    :else (concat [a-seq] [(concat (rest a-seq) [(first a-seq)])]
    )))

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))

; (defn old-book->new-book [book]
;  (assoc book :authors (set (get book :authors)))
;)
;   (assoc book :authors (conj (get book :authors) new-author))


(defn my-frequencies-helper [freqs a-seq]
  (if (contains? freqs (first a-seq))
    (assoc freqs (first a-seq) (set (inc (get freqs (first a-seq)))))
    (assoc freqs (conj freqs ((first a-seq) 1)))
  )
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  [:-])

(defn my-drop [n coll]
  [:-])

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

