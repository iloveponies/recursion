(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll) false
    (empty? (rest coll)) true
    :else false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))
    ))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
    ))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))
      ))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq)))
  )

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()
    ))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty?  a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq
    )) 

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false
    )) 

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))
    ))
      

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (dec n)) (fib (- n 2)))
    ))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    ))

(defn my-range [up-to]
  (cond
    (<= up-to 0) '()
    (= up-to 1) '(0)
    :else (cons (dec up-to)(my-range (dec up-to)))
    ))
           

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (tails (rest a-seq)))
    ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [double-seq (concat a-seq a-seq)
        num (count a-seq)]
    (defn rotations-helper [b-seq]
      (if
          (= num (count b-seq))
        (list b-seq)
        (cons (take num b-seq) (rotations-helper (rest b-seq)))))
    (rotations-helper (rest double-seq))))
    

(defn my-frequencies-helper [freqs a-seq]
  (let [key (first a-seq)
        val (get freqs key)
        rem-seq (rest a-seq)]
    (if (empty? a-seq)
      freqs
      (if val
            (my-frequencies-helper (assoc freqs key (inc val)) rem-seq)
            (my-frequencies-helper (assoc freqs key 1) rem-seq)
            ))))

(defn my-frequencies [a-seq]
   (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    nil
    (let [tuple (first a-map)]
    (concat (my-repeat (second tuple) (first tuple)) (un-frequencies (rest a-map))))
    ))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))
    ))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (dec n) (rest coll))
    ))

(defn halve [a-seq]
  (let [splitpos (int (/ (count a-seq) 2))]
    (cons (my-take splitpos a-seq) (cons (my-drop splitpos a-seq) '()))
    ))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else
    (let
        [a-val (first a-seq)
         b-val (first b-seq)]
      (if (<= a-val b-val)
        (cons a-val (seq-merge (rest a-seq) b-seq))
        (cons b-val (seq-merge a-seq (rest b-seq)))
        ))))

(defn merge-sort [a-seq]
  (if (empty? (rest a-seq))
    a-seq
   (let [halves (halve a-seq)
          firsthalf (first halves)
          secondhalf (second halves)]
      (seq-merge (merge-sort firsthalf) (merge-sort secondhalf))
      )))


(defn split-into-monotonics [a-seq]
  (reverse (map reverse
       (loop [acc nil
              mon nil
              seq1 a-seq
              dirup nil
            ]
         (cond
           (empty? seq1) (cons mon acc)
           (and (empty? (rest seq1)) (empty? mon)) (recur acc (cons (first seq1) nil) (rest seq1) dirup) 
           (or (empty? (rest seq1)) (and (not dirup) (< (first seq1) (second seq1)))) (recur acc (cons (first seq1) mon) (rest seq1) "up")
            (or (empty? (rest seq1)) (and (not dirup) (> (first seq1) (second seq1)))) (recur acc (cons (first seq1) mon) (rest seq1) "down")
           (or (empty? (rest seq1)) (and (= dirup "up") (< (first seq1) (second seq1)))) (recur acc (cons (first seq1) mon) (rest seq1) dirup)
           (or (empty? (rest seq1)) (and (= dirup "down") (> (first seq1) (second seq1)))) (recur acc (cons (first seq1) mon) (rest seq1) dirup)
           :else (recur (cons (cons (first seq1) mon) acc) nil (rest seq1) nil)
           )))))


(defn permutations [a-set]
  (if (empty? a-set)
    '()
   (reduce concat
          (map (fn [x]
         (let [remaining (disj a-set x)]
           (map (fn [rem] (cons x rem )) (rotations remaining))
           )) a-set )
   )))

;take each element in the set and concat it to the the rotations of what is left
;(defn powerset [a-set]
;  (defn powerset-helper [set acc]
;   (if (empty? set)
;    acc
;    (powerset-helper (disj set (first set)) (conj (conj acc (first set)) (disj set (first set))))
;    ))
;  (reduce powerset-helper a-set #{a-set}))
  ; define a helper which adds the first and rest of every elem into the acc set

;(defn powerset [a-set]
;  (defn powerset-helper [elem set acc]
;    (if (empty? set)
;      acc
;       (conj (conj (conj acc (set (list elem))) set) (map (fn [x] (powerset-helper x (disj set x) acc)) set))
;      ))
;  (concat (map (fn [x] (powerset-helper x (disj a-set x) #{a-set})) a-set )))


(defn powerset [a-set]
  (defn powerset-helper-map [m-set acc]
    (map (fn [x] (powerset-helper x (disj m-set x) acc)) m-set))
  (defn powerset-helper [elem set1 acc]
    (if (empty? set1)
      acc
      (let [newacc (conj (conj acc #{elem}) set1)]
        (powerset-helper-map set1 newacc))))
  (powerset-helper-map a-set #{a-set}))

  
(defn powerset [a-set]
  (defn powerset-helper [elem set1 acc]
    (if (empty? set1)
      acc
      (conj (conj acc #{elem}) set1)
      ))
  (let [sets (map (fn [x] (powerset-helper x (disj a-set x) #{a-set})) a-set)]
                                        ;(reduce clojure.set/union (map (fn [x] (conj (second sets) x)) (first sets)))))
    (reduce clojure.set/union sets)))
;test this idea of how to combine down the sets
