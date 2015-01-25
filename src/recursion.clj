(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
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
   :else '[]))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (empty? a-seq) (empty? b-seq)
   (empty? b-seq) (empty? a-seq)
   :else          (if (= (first a-seq) (first b-seq))
                    (seq= (rest a-seq) (rest b-seq))
                    false)))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) ()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (zero? k) 1
   (zero? n) 0
   :else (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) ()
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (zero? up-to) ()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let
    [new-head (fn [head tail] (concat head [(first tail)]))
     rotator (fn rotator [head tail]
                  (cond
                   (empty? tail) '()
                   :else (cons
                          (concat tail head)
                          (rotator (new-head head tail) (rest tail)))))]
    (cond
     (empty? a-seq) '(())
     :else (rotator '() a-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (let
    [element (first a-seq)
     update-freqs (fn [freqs key]
                    (assoc freqs element (inc (if (get freqs element)
                                                (get freqs element)
                                                0))))]
  (cond
   (empty? a-seq) freqs
   :else (my-frequencies-helper
          (update-freqs freqs element)
          (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
   (empty? a-map) a-map
   :else (let [current-key (key (first a-map))
               current-value (val (first a-map))]
           (concat
            (repeat current-value current-key)
            (un-frequencies (rest a-map)) ))))

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

