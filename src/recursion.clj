(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (and (empty? (rest coll))
           (not (empty? coll)))
    true
    false
    ))

(defn my-last [coll]
  (if (empty? coll)
  nil
  (if (singleton? coll)
    (first coll)
    (my-last (rest coll))
    )))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
     (if (singleton? a-seq)
       (first a-seq)
       (max (first a-seq) (max-element (rest a-seq)))
       )))


(defn length-seq  [a-seq]
   (if (empty? a-seq)
     0
     (+ 1 (length-seq (rest a-seq)))))

 (defn seq-max [seq-1 seq-2]
  (if (> (length-seq seq-1) (length-seq seq-2))
    seq-1
    seq-2))


(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
   (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq))
    )))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))
   ))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq
   ))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (not (== (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))
   ))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (cond
   (zero? k) 1
   :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (< n 2) n
   :else (+ (fib (- n 1))
            (fib (- n 2)))
   ))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (<= how-many-times 0) ()
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (<= up-to 0) ()
   :else (cons (dec up-to) (my-range (dec up-to)))
   ))

(defn tails [a-seq]
  (cond
   (empty? a-seq) (cons a-seq ())
   :else (cons a-seq (tails (rest a-seq)))
   ))

(defn inits [a-seq]
 (cond
   (empty? a-seq) (cons a-seq ())
   :else (cons a-seq (map reverse (tails (rest (reverse a-seq)))))
   ))

(defn single-rotation [a-seq cnt pos]
  (cond
   (== pos (count a-seq)) (single-rotation a-seq cnt 0)
   (== cnt (count a-seq)) ()
   :else (cons (get a-seq pos) (single-rotation a-seq (inc cnt) (inc pos)))
   )
  )

(defn rotations-helper [a-seq n]
  (cond
   (== n (count a-seq)) ()
   :else (cons (single-rotation a-seq 0 n)
               (rotations-helper a-seq (inc n)))))


(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-helper a-seq 0)
  ))


(defn my-frequencies-helper [freqs a-seq]
  (let [cnt (fn [freqs key]
        (if (contains? freqs key)
          (freqs key)
           0))]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper
       (assoc freqs (first a-seq) (inc (cnt freqs (first a-seq))))
         (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map ) ""
    (concat (repeat (first (vals a-map))
                    (first (keys a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take-helper [n coll i]
  (if (or (== n i)
          (> i (count coll)))
    ()
    (cons (first coll)
          (my-take-helper n (rest coll) (inc i)))))

(defn my-take [n coll]
  (my-take-helper n coll 0))

(defn my-drop-helper [n coll i]
  (cond
   (empty? coll) ()
   (> n i) (my-drop-helper n (rest coll) (inc i))
   :else (cons (first coll) (my-drop-helper n (rest coll) (inc i)))
   )
  )

(defn my-drop [n coll]
  (my-drop-helper n coll 0))


(defn halve [a-seq]
  (let [mid (fn [a-seq]
              (int (/ (count a-seq) 2)))]
     (cons (my-take (mid a-seq) a-seq)
           (vector (my-drop (mid a-seq) a-seq)))))

(defn seq-merge [a-seq b-seq]
(cond
 (and (empty? a-seq) (empty? b-seq)) a-seq
 (or (empty? a-seq)
     (< (first b-seq)
        (first a-seq))) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))
 :else (cons (first a-seq) (seq-merge (rest a-seq) b-seq))))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

