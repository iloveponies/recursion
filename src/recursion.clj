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
  (if (not (nil? (first a-seq)))
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (nil? (first a-seq))
    nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? f)
        (cons f (my-filter pred? r))
        (my-filter pred? r)))))

(defn sequence-contains? [elem a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond
     (empty? a-seq) false
     (not (== elem f)) (sequence-contains? elem r)
     :else true)))

(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? f) (cons f (my-take-while pred? r))
     :else [])))

(defn my-drop-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? f) (my-drop-while pred? r)
     :else a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (== (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (let [f1 (first seq-1)
          r1 (rest seq-1)
          f2 (first seq-2)
          r2 (rest seq-2)]
      (cons (f f1 f2) (my-map f r1 r2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (== 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    []
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [()]
    (conj (inits (drop-last a-seq)) (seq a-seq))))

(inits [1 2 3 4])

(defn rotations2 [a-seq]
  "ugly, but works for now :)"
  (letfn [(rot [n coll]
            (if (zero? n)
              []
              (cons (concat (drop n coll) (take n coll)) (rot (dec n) coll))))]
    (rot (count a-seq) a-seq)))

(defn rotations [a-seq]
  (loop [acc []
         n (count a-seq)]
    (cond
     (empty? a-seq) (list [])
     (zero? n) acc
     :else (recur (conj acc (concat (drop n a-seq) (take n a-seq))) (dec n)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [new-freqs
          (update-in freqs [(first a-seq)] (fnil inc 0))]
     (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq freqs]
  (if (empty? freqs)
    []
    (let [[k, v] (first freqs)
          new-seq (repeat v k)]
      (concat new-seq (un-frequencies-helper new-seq (rest freqs))))))

(defn un-frequencies [a-map]
  (un-frequencies-helper [] a-map))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [midpoint (int (/ (count a-seq) 2))]
    (vector (take midpoint a-seq) (drop midpoint a-seq))))

(defn seq-merge [[x & xrest :as X] [y & yrest :as Y]]
  (cond
   (empty? X) Y
   (empty? Y) X
   :else 
     (if (< x y)
       (concat [x] (seq-merge xrest Y))
       (concat [y] (seq-merge yrest X)))))

(defn merge-sort [a-seq]
  "If the sequence is 0 or 1 elements long, it is already sorted.
   Otherwise, divide the sequence into two subsequences.
   Sort each subsequence recursively.
   Merge the two subsequences back into one sorted sequence."
  (if (< (count (take 2 a-seq)) 2)
    a-seq
    (let [halve-seq (halve a-seq)]
      (seq-merge (merge-sort (first halve-seq)) (merge-sort (last halve-seq))))))

(defn monotonic-prefix [sequence]
  (let [asc (map <= sequence (next sequence))
      desc (map >= sequence (next sequence))
      asc-true (take-while #(= true %) asc)
      desc-true (take-while #(= true %) desc)
      asc-count (count asc-true)
      desc-count (count desc-true)]
      (if (> asc-count desc-count)
        (take (inc asc-count) sequence)
        (take (inc desc-count) sequence))))

(defn split-into-monotonics [sequence]
  (when-not (empty? sequence)
    (let [mons (monotonic-prefix sequence)]
      (cons mons (split-into-monotonics (drop (count mons) sequence))))))

(defn permutations [s]
  (let [perms (fn [[fst & rest]]
                (map (fn [e] (cons fst e)) (permutations rest)))]
    (cond (empty? s)
          '(())
          (= 1 (count s))
          (list s)
          :else
          (apply concat (map perms (rotations s))))))

(permutations #{})
;=> (())
(permutations #{1 5 3})
;=> ((1 5 3) (5 1 3) (5 3 1) (1 3 5) (3 1 5) (3 5 1))

(defn powerset [s]
  (if (empty? s)
    #{#{}}
    (let [augment-subsets (fn [e T] (map #(set (cons e %)) T))
          e (first s)
          t (rest s)]
      (set (concat (augment-subsets e (powerset t))
                   (powerset t))))))
