(ns recursion)

(defn product [coll]
  (if (empty? coll)
      1
      (* (first coll) (product (rest coll)))))
;5
;(product [1 2 4])
;(product (cons 1 (cons 2 (cons 4 '()))))
;(* 1 (product (cons 2 (cons 4 '()))))
;(* 1 (* 2 (product (const 4 ()))))
;(* 1 (* 2 (* 4 product '())))
;(* 1 (* 2 (* 4 1)))
;(* 1 (* 2 4))
;(* 1 8)
;8

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll) (singleton? coll))
      (first coll)
      (my-last (rest coll))))
;14

(defn max-element [a-seq]
  (if (empty? a-seq)
      nil
      (apply max a-seq)))
;17

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
      seq-2
      seq-1))
;20

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq)
      nil
    (singleton? a-seq)
      (first a-seq)
    :else
      (seq-max
        (first a-seq)
        (longest-sequence (rest a-seq)))))
;23

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else
      (my-filter pred? (rest a-seq))))
;28

(defn sequence-contains? [elem a-seq]
  (cond
     (empty? a-seq)
       false
     (= elem (first a-seq))
       true
     :else
       (sequence-contains? elem (rest a-seq))))
;31

(defn my-take-while [pred? a-seq]
  (if
    (or (empty? a-seq) (not (pred? (first a-seq))))
    ()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))
;35

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))
;39

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (== (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))
;45

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    ()
    (cons
      (f (first seq-1) (first seq-2))
      (my-map f (rest seq-1) (rest seq-2))
     )))
;48

(defn power [n k]
  (if (zero? k)
      1
      (* n (power n (dec k)))))
;52

(defn fib [n]
  (cond
    (zero? n)
      0
    (== 1 n)
      1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))
;60

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
      ()
      (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))
;63

(defn my-range [up-to]
  (if (zero? up-to)
      ()
      (let [dec-up-to (dec up-to)]
        (cons dec-up-to (my-range dec-up-to)))))
;67

(defn tails [a-seq]
  (map (fn[n] (drop n a-seq)) (my-range (+ 1 (count a-seq)))))
;70

(defn inits [a-seq]
  (map (fn[n] (take n a-seq)) (my-range (+ 1 (count a-seq)))))
;73

(defn rotations [a-seq]
  (let [temp (map vector (tails a-seq) (inits a-seq))
        result (rest
                  (map (fn[x] (concat (first x) (second x))) temp))]
    (if (empty? result)
      '(())
      result)))
;78

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
      freqs
      (let [elem (first a-seq)
            new-freqs (if (contains? freqs elem)
                      (assoc freqs elem (inc (get freqs elem)))
                      (assoc freqs elem 1))]
        (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))
;81

(defn un-frequencies [a-map]
  (apply concat (map (fn[k] (my-repeat (get a-map k) k)) (keys a-map))))
;84

(defn my-take [n coll]
  (if (or (empty? coll) (>= 0 n))
      '()
      (cons (first coll) (my-take (dec n) (rest coll)))))
;86

(defn my-drop [n coll]
  (cond
     (== 0 n)
       coll
     (> n (count coll))
       '()
     :else
       (my-drop (dec n) (rest coll))))
;88

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
   (cons (my-take half a-seq) (cons (my-drop half a-seq) '()))))
;91

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
     :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))
;93

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (== 1 (count a-seq)))
      a-seq
      (let [halved (halve a-seq)]
       (seq-merge (merge-sort (first halved)) (merge-sort (second halved))))))
;96

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (if (apply < (take 2 a-seq))
        (let [mono (my-take-while (fn[x] (or (empty? x) (apply <= x))) (reverse (inits a-seq)))
          winner (last mono)]
         (cons winner (split-into-monotonics (drop (count winner) a-seq))))
        (let [mono (my-take-while (fn[x] (or (empty? x) (apply >= x))) (reverse (inits a-seq)))
              winner (last mono)]
         (cons winner (split-into-monotonics (drop (count winner) a-seq)))))))
;98

(defn list-all-except [a-seq x]
  (my-filter (fn[y] (not (== x y))) a-seq))

(defn permutations [a-set]
  (if (empty? a-set)
      '(())
      (apply concat (map (fn[x] (map (fn[perm] (flatten (cons x perm))) (permutations (list-all-except a-set x)))) a-set))))
;101

; all subsets of a-set of size n
(defn filter-set [a-set i]
  (filter (fn[j] (== (bit-and i j) j)) a-set))

(defn powerset [a-set]
  (if (empty? a-set)
    '(())
    (let [pow (long (Math/pow 2 (count a-set)))]
     (map (fn[i] (filter-set a-set i)) (my-range pow)))))

;104
