(ns recursion)

(defn product [coll]
  (if (< (count coll) 1)
    1
    (* (first coll)
       (product (rest coll))))

    )

(defn singleton? [coll]
  (not(or
       (empty? coll)
       (not(empty? (rest coll))))))

(defn my-last [coll]
  (if (empty?(rest coll))
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
  seq-2
  ))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))
  ))

(defn my-filter [pred? a-seq]
   (if  (empty? (rest a-seq))
        (if (pred? (first a-seq))
          (cons (first a-seq) ())
          ())
        (if (pred? (first a-seq))
          (cons (first a-seq) (my-filter pred? (rest a-seq)))
          (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if(empty? (rest a-seq))
    (= (first a-seq) elem)
    (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))

 (defn my-take-while [pred? a-seq]
  (if (empty? (rest a-seq))
    (if (first a-seq)
      (if (pred? (first a-seq))
      (cons (first a-seq) ())
      (cons () ()))
    ())
    (if (pred? (first a-seq))
      (if (empty (my-take-while pred? (rest a-seq)))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      (cons (first a-seq) ()))
      ())))



(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     ()
   (pred? (first a-seq))
     (my-drop-while pred? (rest a-seq))
   (not (pred? (first a-seq)))
     (cons (first a-seq) (rest a-seq))
   ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
 (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    ;(not (f (first a-seq) (first b-seq))) false
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= k 0) 1
   :else (* n (power n (dec k)))
   ))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))

  ))

(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (conj (my-range (dec up-to)) (dec up-to))))


(defn tails [a-seq]

  (if (empty? a-seq)
   (cons () ())
    (cons a-seq (tails (rest a-seq)))))


(defn inits [a-seq]
  (if (empty? a-seq)
   (cons () ())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (let [re-arrange-part (fn [foo] (cond
                                   (== (count foo) 3) (concat (second foo) (last foo) (first foo ))
                                   (== (count foo) 2) (cond
                                                       (< (count (first foo)) (count (second foo))) (concat (first foo) (second foo))
                                                       (> (count (first foo)) (count (second foo))) (concat (second foo) (first foo)))))
        part (fn [z] (partition-by (fn [x] (= x z)) a-seq))
        split (fn [y] (re-arrange-part(part y)))]
  (cond
   (= (count a-seq) 2) (merge (merge () a-seq) (cons (second a-seq) (cons (first a-seq) ())))
   (= (count a-seq) 0) (quote(()))
   :else (map split a-seq)
  )))



(defn my-frequencies-helper [freqs a-seq]
  (let [get-freq (fn [k] (if(get freqs k)(get freqs k) 0))]
  (cond
   (empty? a-seq) freqs
   :else
       (my-frequencies-helper (assoc freqs (first a-seq) (inc (get-freq (first a-seq)))) (rest a-seq))
  )))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [un-freqs a-map]
  (let [keyz (fn [] (first(first a-map)))
        valz (fn [] (second(first a-map)))
        repeatz (fn [] (repeat (valz) (keyz)))]
  (cond
   (empty? a-map) un-freqs
   :rest (un-frequencies-helper (concat un-freqs (repeatz)) (rest a-map)))))


(defn un-frequencies [a-map]
  (un-frequencies-helper () a-map)
  )

(defn my-take [n coll]
  (cond
   (== n 0) ()
   (not (first coll)) ()
   :rest (cons (first coll) (my-take (dec n) (rest coll)))))


(defn my-drop [n coll]
  (cond
   (== n 0) coll
   :rest (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [split-at (fn [] (int (/ (count a-seq) 2)))]
 (vector (my-take (split-at) a-seq) (my-drop (split-at) a-seq))

  ))

(defn seq-merge-helper [index n x-seq]
  (cond
                                       (empty? x-seq) index
                                       (>= (first x-seq) n) index
                                       :rest (seq-merge-helper (inc index) n (rest x-seq))))

(defn seq-merge [a-seq b-seq]


  (let [        get-index (fn [] (seq-merge-helper 0 (first a-seq) b-seq))]

  (cond
   (empty? a-seq) (concat b-seq [])
  :rest (seq-merge (rest a-seq) (concat (my-take (get-index) b-seq) (into [] (cons (first a-seq) [])) (my-drop (get-index) b-seq)))

        )))

(defn merge-sort [a-seq]
  (cond
   (<= (count a-seq) 1) (into () a-seq)
   :rest (seq-merge (merge-sort (first(halve a-seq))) (merge-sort (second(halve a-seq))))
   ))

(defn split-into-monotonics [a-seq]
  (let [index (fn [] (- (count a-seq) 2))]
  (cond
   (<= (count a-seq) 3) (cons (reverse (into () a-seq)) ())
   :rest (concat (split-into-monotonics (my-take (index) a-seq)) (cons  (reverse (into () (my-drop (index) a-seq))) ())))))


(defn permutations-helper [n a-set]
  (cond
   (<= (count a-set) 1) (first a-set)
   :rest (map (fn [x] (permutations-helper x (disj a-set n))) (disj a-set n))))


(defn permutations [a-set]
  (cond
   (<= (count a-set) 2) (first a-set)
   :rest (map (fn [x] (permutations-helper x a-set)) a-set)))


(defn powerset [a-set]
  [:-])


