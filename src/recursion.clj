(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

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
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (let [fst (first a-seq)
          rest-filtered (my-filter pred? (rest a-seq))]
      (if (pred? fst)
        (cons fst rest-filtered)
        rest-filtered))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else []))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false ; prevent empty sequence and sequence with nil head equalling
   :else (and (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0) 1 (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    []
    (let [decr (dec up-to)]
      (cons decr (my-range decr)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq [])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [seq-reversed-inits (tails (reverse a-seq))]
    (reverse (map (fn [s] (reverse s)) seq-reversed-inits))))

(defn rotations [a-seq]
  (if (empty? a-seq) [a-seq]
    (let [tails-seq (tails a-seq)
          inits-seq (inits a-seq)]
      (rest (map (fn [a b] (concat a b)) tails-seq inits-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (let [fst (first a-seq)
          rst (rest a-seq)
          found_freq (get freqs fst)]
      (let [freq (if (= found_freq nil) 1 (+ found_freq 1))
            modified_freqs (assoc freqs fst freq)]
        (my-frequencies-helper modified_freqs rst)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) []
    (let [[k v] (first a-map)
          repeated (repeat v k)
          rst (un-frequencies (rest a-map))]
      (concat repeated rst))))

(defn my-take [n coll]
  (if (or (empty? coll) (== 0 n)) []
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= 0 n)) coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [cnt (count a-seq)
        n (int (/ cnt 2))
        fst-halve (my-take n a-seq)
        snd-halve (my-drop n a-seq)]
    [fst-halve snd-halve]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        :else (let [fst-a (first a-seq)
                    fst-b (first b-seq)]
                (if (< fst-a fst-b)
                  (cons fst-a (seq-merge (rest a-seq) b-seq))
                  (cons fst-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq)) a-seq
    (let [[fst-halve snd-halve] (halve a-seq)]
      (seq-merge (merge-sort fst-halve) (merge-sort snd-halve)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) []
    (let [monotonics (take-while (fn [i] (let [sorted (merge-sort i)]
                                           (or (seq= i sorted) (seq= i (reverse sorted)))))
                                 (inits a-seq))
          fst (first (reverse monotonics))]
    (if (= fst nil) [] (cons fst (split-into-monotonics (my-drop (count fst) a-seq)))))))

(defn permutations [a-set]
  (if (or (singleton? a-set) (empty? a-set)) [a-set]
    (let [rotated (rotations a-set)]
      (apply concat (map (fn [r] (let [perms (permutations (rest r))]
                     (map (fn [p] (cons (first r) p)) perms)))
                     rotated)))))


(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
    (let [fst (first a-set)
          rst (powerset (rest a-set))
          sets (set (map (fn [pset] (set (cons fst pset))) rst))]
      (set (cons #{fst} (concat rst sets))))))


