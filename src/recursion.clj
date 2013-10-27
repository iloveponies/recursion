(ns recursion)

(defn truthy? [val]
  (not (or (false? val)
           (nil? val))))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


;Exerc 2
;(product [1 2 4])
;(* 1 (product [2 4]))
;(* 1 (* 2 (product [4])))
;(* 1 (* 2 (* 4 (product []))))
;(* 1 (* 2 (* 4 1)))
;(* 1 (* 2 4))
;(* 1 8)
;8


(defn singleton? [coll]
  (and (not (nil? (first coll)))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if(empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (max (first a-seq)
           (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if(empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cond (first a-seq) (my-drop-while pred? (rest a-seq)))
   :else a-seq))

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
   :else (cons
          (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k) 1
   (== 1 k) n
   (== -1 k) (/ 1 n)
   (pos? k) (* n (power n (dec k)))
   (neg? k) (/ 1 (power n (- k))) ;n^-k = 1/n^k
   :else nil))

(defn fib [n]
  (if (<= n 1) n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits[a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotate-helper [n a-seq vec]
  (let [rotated (cons
              (first (reverse a-seq))
              (reverse (rest (reverse a-seq))))]
    (cond
     (empty? a-seq) '(())
     (not (zero? n)) (rotate-helper (dec n) rotated (concat vec (vector rotated)))
     :else vec)))

(defn rotations [a-seq]
  (rotate-helper (count a-seq) a-seq [])) ;Is this possible without (my own) helper function?

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs

    (contains? freqs (first a-seq))
         (my-frequencies-helper
           (assoc freqs (first a-seq) (inc (freqs (first a-seq))))
           (rest a-seq))

    :else (my-frequencies-helper
          (assoc freqs (first a-seq) 1)
          (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (concat
      (repeat (val (first a-map)) (key (first a-map)))
      (un-frequencies (dissoc a-map (key (first a-map))))
   )))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    []
    (concat
     (conj [] (first coll))
     (my-take (dec n) (rest coll))
     )))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [mid (int (/ (count a-seq) 2))]
    (vector
     (my-take mid a-seq)
     (my-drop mid a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq)) (conj (seq-merge (rest a-seq) b-seq) (first a-seq))
   :else (conj (seq-merge (rest b-seq) a-seq) (first b-seq) )))

(defn merge-sort [a-seq]
  (cond
    (>= 1 (count a-seq)) a-seq
    :else (let [half-vec (halve a-seq)
          a (first half-vec)
          b (first (rest half-vec))]
          (seq-merge (merge-sort a) (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

