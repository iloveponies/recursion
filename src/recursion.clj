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

(defn max-element [coll]
   (cond
     (empty? coll) nil
     (singleton? coll) (first coll)
     :else (max (first coll) (max-element (rest coll)))))

(defn seq-max [seq-1 seq-2]
  (letfn [(helper [xs ys]
                 (cond
                   (empty? xs) seq-2
                   (empty? ys) seq-1
                   :else (helper (rest xs) (rest ys))))]
    (helper seq-1 seq-2)))

(defn longest-sequence [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (seq-max (first coll) (longest-sequence (rest coll)))))

(defn my-filter [pred? coll]
  (cond
    (empty? coll) coll
    (pred? (first coll)) (cons (first coll) (my-filter pred? (rest coll)))
    :else (my-filter pred? (rest coll))))


(defn sequence-contains? [elem coll]
  (cond
    (empty? coll) false
    (= (first coll) elem) true
    :else (sequence-contains? elem (rest coll))))

(defn my-take-while [pred? coll]
  (cond
    (empty? coll) coll
    (pred? (first coll)) (cons (first coll) (my-take-while pred? (rest coll)))
    :else []))

(defn my-drop-while [pred? coll]
  (cond
    (empty? coll) coll
    (pred? (first coll)) (my-drop-while pred? (rest coll))
    :else coll))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) []
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= k 0) 1
    :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (<= n 1) n
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) []
    :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))


(defn my-range [up-to]
  (cond
    (<= up-to 0) []
    :else (cons (- up-to 1) (my-range (- up-to 1)))))


(defn tails [coll]
   (cond
     (empty? coll) [coll]
     :else (cons coll (tails (rest coll)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn- rotations-helper [xs ys]
  (cond
    (empty? xs) xs
    :else (let [nxs (rest xs)
                nys (cons (first xs) ys)]
            (cons (concat nxs (reverse nys)) (rotations-helper nxs nys)))))

(defn rotations [a-seq]
  (if (empty? a-seq) [[]] (rotations-helper a-seq [])))

(defn- incr-key [dict key]
    (assoc dict key (+ (or (get dict key) 0) 1)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (incr-key freqs (first a-seq)) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn- un-frequencies-helper [mp]
  (cond
    (empty? mp) []
    :else (let [[val cnt] (first mp)]
            (cons (repeat cnt val) (un-frequencies-helper (rest mp))))))

(defn un-frequencies [mp]
  (apply concat (un-frequencies-helper mp)))

(defn my-take [n coll]
  (cond
    (<= n 0) []
    (empty? coll) []
    :else (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (<= n 0) coll
    (empty? coll) []
    :else (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [cnt (int (/ (count a-seq) 2))]
    [(my-take cnt a-seq) (my-drop cnt a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (empty? a-seq) a-seq
    (singleton? a-seq) a-seq
    :else (let [[l r] (halve a-seq)]
        (seq-merge (merge-sort l) (merge-sort r)))))

(defn- monotonic-helper [is-asc acc xs]
  (cond
    (empty? xs) [(reverse acc) xs]
    (and is-asc (< (first xs) (first acc))) [(reverse acc) xs]
    (and (not is-asc) (> (first xs) (first acc))) [(reverse acc) xs]
    :else (monotonic-helper is-asc (cons (first xs) acc) (rest xs))))

(defn split-into-monotonics [xs]
  (cond
    (or (empty? xs) (singleton? xs)) xs
    :empty (let [[y ys] (monotonic-helper (apply < (my-take 2 xs)) (reverse (my-take 2 xs)) (my-drop 2 xs))]
      (cons y (split-into-monotonics ys)))))

(defn- perm-helper [xs ys]
  (cond
    (empty? ys) []
    :else (cons [(first ys) (concat xs (rest ys))] (perm-helper (cons (first ys) xs) (rest ys)))))

(defn permutations [xs]
  (if (empty? xs) [[]]
    (apply concat (map (fn [[y ys]] (map #(cons y %) (permutations ys))) (perm-helper [] xs)))))

(defn powerset [a-set]
  (cond
    (empty? a-set) #{#{}}
    :else (let [sub-ps (powerset (rest a-set))]
            (clojure.set/union sub-ps (map #(conj % (first a-set)) sub-ps)))))
