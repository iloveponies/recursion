
(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

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
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if
   (empty? (rest a-seq)) (if (pred? (first a-seq))
                           a-seq [])
   (if (pred? (first a-seq))
          (concat [(first a-seq)] (my-filter pred? (rest a-seq)))
          (concat [] (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
  (cond
   (empty? a-seq) ()
   (pred? fst) (concat [fst] (my-take-while pred? rst))
   :else ())))

(defn my-drop-while [pred? a-seq]
  (let [fst (first a-seq)
        rst (rest a-seq)]
    (cond
     (empty? a-seq) ()
     (pred? fst) (concat () (my-drop-while pred? rst))
     :else a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if
   (or (empty? seq-1) (empty? seq-2)) ()
   (concat [(f (first seq-1) (first seq-2))]
           (my-map f (rest seq-1) (rest seq-2)))))

(my-map + [1 2 3] [4 4 4])
(my-map + [1 2 3] [])

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))


(defn fib [n]
  (cond
   (< n 1) 0
   (< n 3) 1
   :else (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) ()
    (concat [what-to-repeat] (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) ()
        (concat [(dec up-to)] (my-range (dec up-to)))))

(my-range 0)
(my-range 1)
(my-range 2)

(defn tails [a-seq]
  (map (fn [n] (drop n a-seq)) (range 0 (inc (count a-seq)))))


(defn inits [a-seq]
  (map (fn [n] (take n a-seq)) (range 0 (inc (count a-seq)))))


(map + [1 2 3 4] [1 2 3 2 5])

(tails [1 2 3 4])

(drop 2 [1 2 3 4])


(defn rotations [a-seq]
  (if (empty? a-seq)
    [()]
  (map (fn [n]
         (concat (drop n a-seq) (take n a-seq)))
         (range 0 (count a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [[x xs] a-seq]
  (if
   (empty? a-seq) freqs
   (my-frequencies-helper (assoc freqs x (if (contains? freqs x)
                    (inc (get freqs x))
                    1)) (rest a-seq))
  )))
(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [rep-key (fn [entry]
                  (repeat (val entry) (key entry)))]
    (apply concat (map rep-key a-map))))

(repeat 3 "a")


(un-frequencies (my-frequencies [:a :b :c :a]))

(defn my-take [n coll]
  (cond (zero? n) ()
        (empty? coll) ()
        :else (concat [(first coll)] (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond (zero? n) coll
        (empty? coll) ()
        :else (my-drop (dec n) (rest coll))))

(my-drop 2 [1 2 3 4])
(my-drop 4 [:a :b])


(int (/ 1 2))

(defn halve [a-seq]
  (let [len (count a-seq)
        head (int (/ len 2))]
    [(take head a-seq) (drop head a-seq)]))


(defn seq-merge [a-seq b-seq]
  (let [a-head (first a-seq)
        b-head (first b-seq)
        a-tail (rest a-seq)
        b-tail (rest b-seq)]
    (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     (<= a-head b-head) (concat [a-head] (seq-merge a-tail b-seq))
     :else (concat [b-head] (seq-merge a-seq b-tail)))))

(defn merge-sort [a-seq]
  (let [[left right] (halve a-seq)]
   (if (>= 1 (count a-seq)) a-seq
        (seq-merge (merge-sort left) (merge-sort right))
   )))

(defn find-series' [f acc a-seq]
  (cond
   (empty? a-seq) 0
   (= 1 (count a-seq)) (+ 1 acc)
   (f (first a-seq) (first (rest a-seq))) (find-series' f (+ 1 acc) (rest a-seq))
   :else (+ 1 acc)
  ))

(defn find-series [f a-seq]
  (find-series' f 0 a-seq))

(defn split-into-monotonics [a-seq]
  (let [asc (find-series < a-seq)
        desc (find-series > a-seq)
        longest (if (> asc desc) asc desc)]
    (if (empty? a-seq)
      a-seq
   (concat [(take longest a-seq)] (split-into-monotonics (drop longest a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
  [a-set]
   (mapcat (fn [n]
         (map (fn [p] (concat [n] p))
              (permutations (filter (fn [e] (not (= e n))) a-set))))
       a-set)))

(defn powerset [a-set]
:-)

(powerset #{1 2 4})

(powerset #{1 2 3})

(powerset #{})
