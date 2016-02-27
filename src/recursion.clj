(ns recursion)

(defn product [coll]
  (if (empty? coll)
   1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
   nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
   nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)(my-last (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
   nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
   a-seq
    (let [fst (first a-seq)
          rst (my-filter pred? (rest a-seq))]
      (if (pred? fst) (cons fst rst) rst))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) ()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (or (empty? a-seq) (empty? b-seq)) (and (empty? a-seq) (empty? b-seq))
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
   ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
   1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
   ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
   ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
   [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
   [[]]
    (conj (inits (butlast a-seq)) a-seq)))

(defn rotations-helper [a-seq times]
 (if (> times 1)
    (cons a-seq (rotations-helper (concat (rest a-seq) (take 1 a-seq)) (dec times)))
    [a-seq]))

(defn rotations [a-seq]
  (rotations-helper a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
   freqs
    (let [rfreqs (my-frequencies-helper freqs (rest a-seq))]
      (assoc rfreqs (first a-seq) (inc (get rfreqs (first a-seq) 0))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
   a-map
    (let [[value repeats] (first a-map)]
      (concat (repeat repeats value) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
   []
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (pos? n)
   (my-drop (dec n) (rest coll))
    coll))

(defn halve [a-seq]
  (let [halv (int (/ (count a-seq) 2))]
   [(my-take halv a-seq) (my-drop halv a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [start (take-while (fn [x] (< x (first a-seq))) b-seq)
                end (drop (count start) b-seq)]
            (concat start (take 1 a-seq) (seq-merge (rest a-seq) end)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[start end] (halve a-seq)]
      (seq-merge (merge-sort start) (merge-sort end)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (if (<= (count a-seq) 2)
   [a-seq]
    (let [ms (take-while monotonic? (rest (inits a-seq)))]
      (cons (last ms) (split-into-monotonics (drop (count ms) a-seq))))))

(defn permutations [a-set]
  (cond
   (empty? a-set) [[]]
    (= 1 (count a-set)) (list a-set)
    :else (for [a a-set
                b (permutations (disj (set a-set) a))]
            (cons a b))))

(defn powerset [a-set]
  (apply clojure.set/union
        #{(set a-set)}
         (map (fn [a] (powerset (disj (set a-set) a))) (set a-set))))

