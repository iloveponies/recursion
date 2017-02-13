(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))
  )
)

(defn singleton? [coll]
  (if (empty? coll) false
    (if (empty?(rest coll)) true false)
  )
)

(defn my-last [coll]
  (last coll)
)

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (apply max a-seq)
  )
)

(defn seq-max [seq-1 seq-2]
  (let [eka (count seq-1)
       toka (count seq-2)]
  (if (> eka toka) seq-1 seq-2)

  )
)

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))
  )
)

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
(cond
  (empty? a-seq) false
  (= elem (first a-seq)) true
  :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
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
  (or (empty? seq-2) (empty? seq-1)) '()
  :else
  (cons (f (first seq-1) (first seq-2))
  (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))
  )
)

(defn fib [n]
  (cond
   (zero? n) 0
   (== n 1) 1
   :else
   (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (> 1 how-many-times) '()
    :else
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat)))
)

(defn my-range [up-to]
(cond
    (> 1 up-to) '()
    :else (cons (dec up-to) (my-range (dec up-to)))
)
)

(defn tails [a-seq]
(cond
    (empty? a-seq) '(())
    :else
    (cons a-seq (tails (rest a-seq)))
)
)

(defn inits [a-seq]
(map reverse (tails (reverse a-seq))))

(defn rotations-apu [a-seq nro]
  (if (> nro 1)
    (cons a-seq (rotations-apu (concat (rest a-seq) (take 1 a-seq)) (dec nro)))
    [a-seq]
  )
)

(defn rotations [a-seq]
  (rotations-apu a-seq (count a-seq))
)

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [rfregs (my-frequencies-helper freqs (rest a-seq))]
    (assoc rfregs (first a-seq) (inc (get rfregs (first a-seq) 0)))
    )
  )
)

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
   a-map
    (let [[value i] (first a-map)]
    (concat (repeat i value) (un-frequencies (rest a-map))))
  )
)

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
   ()
    (cons (first coll) (my-take (dec n) (rest coll)))
  )
)

(defn my-drop [n coll]
  (if (pos? n)
    (my-drop (dec n) (rest coll))
    coll
  )
)

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]
  )
)


(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [start (take-while (fn [x] (< x (first a-seq))) b-seq)
                end (drop (count start) b-seq)]
            (concat start (take 1 a-seq) (seq-merge (rest a-seq) end))
          )
  )
)

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (= 1 (count a-seq)))
    a-seq
    (apply seq-merge (map merge-sort (halve a-seq)))
  )
)

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  (apply clojure.set/union
        #{(set a-set)}
         (map (fn [a] (powerset (disj (set a-set) a))) (set a-set))
  )
  ; disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  ;does not contain key(s).
  ;set returns a set of the distinct elements of coll.
)

