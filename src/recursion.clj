(ns recursion)

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
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq)
          (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq)(longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let[x (first a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? x)
        (cons x (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))
)))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))
))

(defn my-take-while [pred? a-seq]
  (let [x (first a-seq)]
  (if (empty? a-seq) '()
  (if (pred? x)
    (cons x (my-take-while pred? (rest a-seq)))
    '()
))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) a-seq
  (if (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
    a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (empty? a-seq) false
    (empty? b-seq) false
    (= (first a-seq)(first b-seq)) (seq= (rest a-seq)(rest b-seq))
    :else false
))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))    
))

(defn sqr [n]
  (* n n)
)

(defn power [n k]
  (if (= k 0) 1
    (if (= (mod k 2) 0)
      (sqr (power n (/ k 2)))
      (* n (power n (- k 1)))
)))

(defn fib [n]
  (cond
    (<= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))
))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))
))

(defn my-range [up-to]
  (let [x (- up-to 1)]
  (if (< x 0)
    '()
    (cons x (my-range x))
)))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons '() '())
    (rest (my-map concat (reverse (tails a-seq)) (inits a-seq)))
))

(defn mapinc [freqs x]
  (if (contains? freqs x)
    (assoc freqs x (+ 1 (get freqs x))) ;inc
    (assoc freqs x 1) ;put
))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (mapinc freqs (first a-seq)) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
 (let [[k v] (first a-map)]
  (if (empty? a-map)
    '()
    (concat (my-repeat v k) (un-frequencies (rest a-map)))
)))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    coll
    (my-drop (- n 1) (rest coll))
))

(defn halve [a-seq] ; slow, I know
 (let [n (int (/ (count a-seq) 2))]
  (cons (my-take n a-seq) (cons (my-drop n a-seq) '())
)))

(defn seq-merge [a-seq b-seq]
 (let [[a b][(first a-seq) (first b-seq)]]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (if (< a b)
         (cons a (seq-merge (rest a-seq) b-seq))
         (cons b (seq-merge a-seq (rest b-seq)))
))))

(defn merge-sort [a-seq]
 (if (or (empty? a-seq) (singleton? a-seq))
  a-seq
  (let [[h1 h2] (halve a-seq)]
   (seq-merge (merge-sort h1) (merge-sort h2))
)))

(defn monotonic? [a-seq]
 (cond
  (empty? a-seq) nil
  (or (apply <= a-seq) (apply >= a-seq)) a-seq
  :else nil
))

;(defn split-into-monotonics [a-seq]())

(defn split-into-monotonics [a-seq]
 (cond
  (empty? a-seq) '()
  (singleton? a-seq) a-seq
  :else
  (let [mono (some monotonic? (inits a-seq))]
   (cons mono (split-into-monotonics (drop (count mono) a-seq))
))))

;http://stackoverflow.com/questions/1651351/clojure-call-a-function-for-each-element-in-a-vector-with-it-index
; (let [idv (map vector (iterate inc 0) data)]
;  (doseq [[index value] idv] (setCell 0 index value)))

(defn seq-permutations [a-seq]
 (if (empty? a-seq)
  (cons '() '())
   (apply concat (for [x a-seq]
    (let [f (fn [p] (conj p x)) new-seq (remove #{x} a-seq)]
     (map f (seq-permutations new-seq)
))))))

(defn permutations [a-set]
 (seq-permutations (into '() a-set)
))

;for all x in set
; remove x from set, map (cons x) permutations set

(defn seq-powerset [a-seq]
 (if (empty? a-seq)
  #{}
  ;combine (powerset rest) with (powerset rest with first)

))

(defn powerset [a-set]
 (if (empty? a-set)
  (cons '() '())
  (let [addfirst (fn [s] (cons (first a-set) s))
        r        (powerset (rest a-set))]
   (concat (map addfirst r) r
))))








