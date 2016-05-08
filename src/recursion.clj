(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond (empty? coll) false
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
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not= (empty? a-seq) (empty? b-seq)) false
    (not= (first a-seq) (first b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) ()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
    (= k 0) 1
    (= k 1) n
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 1) ()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= up-to 0) ()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () ())
    (drop 1 (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [key (first a-seq)
        val (freqs key)]
    (if (empty? a-seq)
      freqs
      (if (contains? freqs key)
        (my-frequencies-helper (assoc freqs key (inc val)) (rest a-seq))
        (my-frequencies-helper (assoc freqs key 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [acc a-map]
  (let [fst (first a-map)
        rst (rest a-map)
        key (first fst)
        val (second fst)]
    (if (empty? a-map)
      acc
      (un-frequencies-helper (cons (repeat val key) acc) rst))))

(defn un-frequencies [a-map]
  (apply concat (un-frequencies-helper [] a-map)))

(defn my-take-helper [n-curr acc n coll]
  (cond (empty? coll) acc
        (>= n-curr n) acc
        :else (my-take-helper (inc n-curr) (cons (first coll) acc) n (rest coll))))

(defn my-take [n coll]
  (reverse (my-take-helper 0 [] n coll)))

(defn my-drop-helper [n-curr n coll]
  (cond (empty? coll) ()
        (>= n-curr n) coll
        :else (my-drop-helper (inc n-curr) n (rest coll))))

(defn my-drop [n coll]
  (my-drop-helper 0 n coll))

(defn halve [a-seq]
  (let [len (count a-seq)
        nleft (int (/ len 2))]
    [(my-take nleft a-seq) (my-drop nleft a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a-fst (first a-seq)
        b-fst (first b-seq)]
    (cond (nil? a-fst) b-seq
          (nil? b-fst) a-seq
          :else (if (< a-fst b-fst)
                  (cons a-fst (seq-merge (rest a-seq) b-seq))
                  (cons b-fst (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [[ls rs] (halve a-seq)]
      (seq-merge (merge-sort ls) (merge-sort rs)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    a-seq
    (let [first-seq (last (take-while (fn [x] (or (apply <= x) (apply >= x))) (drop 1 (inits a-seq))))
          first-count (count first-seq)
          rst (drop first-count a-seq)]
      (cons first-seq (split-into-monotonics rst)))))

;; http://stackoverflow.com/questions/26076077/clojure-list-all-permutations-of-a-list?lq=1
(defn permutations [a-set]
  (cond (empty? a-set) (cons () ())
        (== 1 (count a-set)) (list a-set)
        :else (for [fst a-set
                    rst (permutations (disj (set a-set) fst))]
                (cons fst rst))))

;; http://codereview.stackexchange.com/questions/12979/powerset-in-clojure
(defn powerset [a-set]
  (if (empty? a-set) '(())
    (let [next-powerset (powerset (next a-set))
          first-el (first a-set)]
      (clojure.set/union next-powerset (map (fn [x] (conj x first-el )) next-powerset)))))
