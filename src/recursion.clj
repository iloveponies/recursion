(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (cond (empty? coll) false
        (empty? (rest coll)) true
        :else false))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (= 1 (count coll))
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq] ; ei tehty ?
  (filter pred? a-seq))



(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (not (= elem (first a-seq))) (sequence-contains? elem (rest a-seq))
        :else true))

(defn my-take-while [pred? a-seq] ; ei tehty?
  (take-while pred? a-seq))

(defn my-drop-while [pred? a-seq] ; ei tehty?
  (drop-while pred? a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (not= (count a-seq) (count b-seq)) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    (list)
    (cons (f (first seq-1)(first seq-2))(my-map f (rest seq-1)(rest seq-2)))))


(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
               (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    (list)
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))



(defn my-range [up-to]
  (if (= 0 up-to)
    (list)
    (cons (dec up-to)(my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (inits (drop-last a-seq)))))

(defn rotations [a-seq] ; ei tehty
  [:-])

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (if (find freqs (first a-seq))
      (my-frequencies-helper (update-in freqs [(first a-seq)] inc) (rest a-seq))
      (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [ufreq (fn [seq] (repeat (get seq 1)(get seq 0)))]
      (concat (ufreq (first a-map)) (un-frequencies (rest a-map))))))


(defn my-take [n coll] ;?
  (take n coll))

(defn my-drop [n coll] ;?
  (drop n coll))

(defn halve [a-seq]
  (if (empty? a-seq)
    (list)
  (let [div (int (/ (count a-seq) 2))]
    [(my-take div a-seq) (my-drop div a-seq)])))

(defn s-merge [o a b]
  (cond (zero? (count a)) (concat o b)
        (zero? (count b)) (concat o a)
        (> (first a) (first b)) (s-merge (conj o (first b)) a (rest b))
        (< (first a) (first b)) (s-merge (conj o (first a)) (rest a) b)
        (= (first a) (first b)) (s-merge (conj o (first a)(first b)) (rest a) (rest b))))

(defn seq-merge [a-seq b-seq]
  (s-merge [] a-seq b-seq))



(defn merge-sort [a-seq]
  (let [[e & t] a-seq]
    (if (nil? t)
      a-seq
      (let [[ekat tokat] (halve a-seq)]
        (seq-merge (merge-sort ekat)(merge-sort tokat))))))



(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

