(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))


(defn singleton? [coll]
  (and (not(empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if(not(empty? a-seq))
    (seq-max (first a-seq) (longest-sequence(rest a-seq)))
    nil))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    ()
  (if (pred? (first a-seq))
    (cons (first a-seq) (my-filter pred? (rest a-seq)))
    (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (== elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))
   ))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
   (pred? (first a-seq))(my-drop-while pred? (rest a-seq))
   :else (cons (first a-seq) (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq)(empty? b-seq)) true
   (or (empty? a-seq)(empty? b-seq)) false
   (= (first a-seq)(first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1)(empty? seq-2)) ()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1) ()
   :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    ()
    (cons (- up-to 1) (my-range (- up-to 1)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () (rest a-seq))
    (cons (cons (first a-seq) (rest a-seq)) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rest (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
     (conj freqs {})
     (conj {(first a-seq) (count (filter (fn [x] (= (first a-seq) x)) a-seq))}
           (my-frequencies-helper freqs (remove (fn [x] (= (first a-seq) x)) a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    {}
    (concat (repeat  (first(rest(first a-map))) (first(first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (= n 0) (empty? coll))
    ()
    (concat (vector (first coll)) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (< n (count coll))
    (reverse(my-take n (reverse coll)))
    ()))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    (if (= n 0)
      (vector () (my-take (count a-seq) a-seq))
      (vector (my-take n a-seq) (my-drop (- (count a-seq) n) a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
       (empty? a-seq) b-seq
       (empty? b-seq) a-seq
       :else (let [smaller? (fn [x] (<= x (first a-seq)))]
               (let [ seq-beginning (concat (my-take-while smaller? b-seq) [(first a-seq)])]
                 (concat seq-beginning (seq-merge (rest a-seq) (my-drop-while smaller? b-seq) ))))))


(defn perm [b-seq]
    (cond
     (= 2 (count b-seq)) (rotations b-seq)
     :else (map (fn [a-seq] (map (fn [x] (concat (cons (first a-seq) ()) x)) (perm (rest a-seq)))) (rotations b-seq))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) ()
   (< (count a-seq) 2)a-seq
   :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))


(defn permut [a-seq]
  (if (empty? a-seq)
    a-seq
    (apply concat (first (perm a-seq)) (rest(perm a-seq)))))

(defn per [n a-seq]
  (if (< n (count a-seq))
    (if (= 3 (count a-seq))
      (permut a-seq)
      (concat (map (fn [x] (concat (cons (first a-seq) ()) x)) (per 0 (rest a-seq)))
            (per (+ n 1) (concat (rest a-seq) (cons (first a-seq) ())))))))

(defn permutations [a-seq]
  (cond
   (empty? a-seq) '(())
   (= 1 (count a-seq)) (cons (first a-seq) ())
   (= 2 (count a-seq)) (rotations a-seq)
   :else (per 0  a-seq)))







(defn split-into-monotonics [a-seq]
  [:-])


(defn powerset [a-set]
  [:-])

