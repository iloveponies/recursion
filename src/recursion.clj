(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

; (product '(1 2 4))
; (product (cons 1 (cons 2 (cons 4 '()))))
; (* 1 (product (cons 2 (cons 4 '()))))
; (* 1 (* 2 (product (cons 4 '()))))
; (* 1 (* 2 (* 4 (product '()))))
; (* 1 (* 2 (* 4 1)))
; (* 1 (* 2 4))
; (* 1 8)
; 8

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (empty? (rest coll)) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [fun (fn fun [seq-1 seq-2]
              (cond (empty? seq-1) 'second
                    (empty? seq-2) 'first
                    :else (fun (rest seq-1) (rest seq-2))))]
    (if (= (fun seq-1 seq-2) 'first)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (let [fun (fn fun [best a-seq]
              (if (empty? a-seq)
                best
                (fun (seq-max best (first a-seq))
                     (rest a-seq))))]
    (if (empty? a-seq)
      nil
      (fun (first a-seq) (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq))
          (cons (first a-seq)
                (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (not (pred? (first a-seq))) '()
        :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (empty? a-seq) (empty? b-seq)
        (empty? b-seq) (empty? a-seq)
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))
; naive
(defn power [n k]
  (cond (zero? k) 1
        (even? k) (let [x (power n (/ k 2))]
                    (* x x))
        :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond (not (pos? n)) 0
        (<= 1 n 2) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (pos? how-many-times)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))
    '()))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [inits-1 (fn fun [init a-seq]
                  (if (empty? a-seq)
                   '()
                    (let [new-init (cons (first a-seq) init)]
                      (cons new-init (fun new-init (rest a-seq))))))
        reverse-seqs (fn fun [a-seq]
                       (if (empty? a-seq)
                        '()
                         (cons (reverse (first a-seq)) (fun (rest a-seq)))))]
    (reverse-seqs (cons '() (inits-1 '() a-seq)))))


(defn rotations [a-seq]
  (let [rotate (fn [a-seq]
                 (reverse (cons (first a-seq) (reverse (rest a-seq)))))
        do-rotations (fn fun [n a-seq]
                       (if (zero? n)
                         '()
                         (cons a-seq (fun (dec n) (rotate a-seq)))))]
    (if (empty? a-seq)
      '(())
      (do-rotations (count a-seq) a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [k (first a-seq)
          v (if (contains? freqs k) (inc (freqs k)) 1)]
      (my-frequencies-helper (assoc freqs k v) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (dissoc a-map k))))))

(defn my-take [n coll]
  (if (and (pos? n)
           (not (empty? coll)))
    (cons (first coll) (my-take (dec n) (rest coll)))
    '()))

(defn my-drop [n coll]
  (if (and (pos? n)
           (not (empty? coll)))
    (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< (first a-seq) (first b-seq))
          (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else
          (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq)
          (not (second a-seq)))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [monotonic? (fn [a-seq] (or (apply >= a-seq) (apply <= a-seq)))
          init (last (take-while monotonic? (rest (inits a-seq))))]
      (cons init (split-into-monotonics (drop (count init) a-seq))))))

(defn cons-all [elem seqs]
  (map (fn [sq] (cons elem sq)) seqs))

(defn perm-1 [a-seq]
  (if (singleton? a-seq)
    (list a-seq)
    (let [rest-permutations (fn [sq] (cons-all (first sq) (perm-1 (rest sq))))]
      (apply concat (map rest-permutations (rotations a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (perm-1 a-set)))

(defn powerset-1 [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [smaller-sets (map (fn [elem] (disj a-set elem)) a-set)
          sub-sets (map powerset-1 smaller-sets)]
      (set (conj (apply concat (set sub-sets)) a-set)))))

(defn powerset [a-seq]
  (if (set? a-seq)
    (powerset-1 a-seq)
    (powerset-1 (set a-seq))))
