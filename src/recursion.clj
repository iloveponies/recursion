(ns recursion)

; Begins inspecting coll. If coll is empty, return (multiply by) 1
; Else multiply the first element of coll recursively with the rest
; of coll.
(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
    (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (or (empty? coll)(singleton? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq)
              (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq)
                  (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons
                          (first a-seq)
                          (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem
                             (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons
                          (first a-seq)
                          (my-take-while pred?
                                         (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred?
                                        (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
  (and (empty? a-seq)
       (empty? b-seq)) true
  (= a-seq b-seq) (seq= (rest a-seq)
                        (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (and (seq seq-1)
        (seq seq-2)) (cons
                      (f (first seq-1)
                         (first seq-2))
                      (my-map f (rest seq-1)
                                (rest seq-2)))
   :else []))

(defn power [n k]
  (cond
   (= 0 k) 1
   :else (* n (power n (dec k)))
   ))

(defn fib [n]
  (cond
   (= 0 n) 0
   (= 1 n) 1
   :else (+ (fib (dec n))
            (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (>= 0 how-many-times) []
   :else (cons what-to-repeat
            (my-repeat (dec how-many-times)
                       what-to-repeat))))

(defn my-range [up-to]
  (cond
   (>= 0 up-to) []
   :else (cons (dec up-to)
               (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) [[]]
   :else (cons a-seq
               (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
   (empty? a-seq) [[]]
   :else (cons []
               (map (fn [x] (cons (first a-seq) x))
                     (inits (rest a-seq))))))

; gotta go fast
(defn giga-rotator-300 [n a-seq]
  (cond
   (= 1 n ) [a-seq]
   :else   (cons
            a-seq
            (giga-rotator-300
             (dec n)
             (concat
              (rest a-seq)
              [(first a-seq)])))))

(defn rotations [a-seq]
  (cond
   (empty? a-seq) [[]]
   :else (giga-rotator-300 (count a-seq) a-seq)))          ;=> Here we go!!

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-count (if (contains? freqs (first a-seq))
                      (assoc freqs (first a-seq) (inc (get freqs (first a-seq))))
                      (merge freqs {(first a-seq) 1}))]
      (my-frequencies-helper new-count (rest a-seq))
      )))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    a-map
    (let [repeater (take (get (first a-map) 1)
          (repeat (get (first a-map) 0)))]
      (concat repeater (un-frequencies (rest a-map)))
      )))

(defn my-take [n coll]
  (if (or (empty? coll) (= 0 n))
  ()
  (cons (first coll)
        (my-take
         (dec n)
         (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= 0 n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half-of-set (int (/ (count a-seq) 2))]
    [(my-take half-of-set
              a-seq)
     (my-drop half-of-set
              a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else
   (let [x (first a-seq)
         y (first b-seq)]
     (if (< x y )
       (cons x (seq-merge (rest a-seq) b-seq))
       (cons y (seq-merge (rest b-seq) a-seq))))))

(defn merge-sort [a-seq]
  (if (> 2 (count a-seq))
    a-seq
    (let [[x y] (halve a-seq)]
      (seq-merge (merge-sort x) (merge-sort y)))
   ))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

