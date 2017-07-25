(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
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
  (cond
    (empty? a-seq) nil
    (empty? (rest a-seq)) (first a-seq)
    (singleton? (rest a-seq)) (max (first a-seq) (first (rest a-seq)))
    :else (max(first a-seq) (max-element(rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if(<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? (rest a-seq))(seq-max (first a-seq)(first(rest a-seq)))
    :else (seq-max(first a-seq)(longest-sequence(rest a-seq)))))

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
    ((complement pred?)(first a-seq))()
    :else (cons(first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    ((complement pred?) (first a-seq)) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1)(first seq-2)) (my-map f (rest seq-1)(rest seq-2)))))

(defn power [n k]
  (cond
    (zero? k) 1
    (zero? n) 0
    :else (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) ()
    :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (zero? up-to) ()
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) (cons a-seq ())
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
   (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
     (cond
       (empty? a-seq) (cons a-seq ())
       :else (rest (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq) 0))) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
    (empty? a-map) '()
    :else (concat (repeat (second (first a-map))(first (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
   (= (min n (count coll))  0) ()
   :else (cons (first coll) (my-take (dec n)(rest coll)))))

(defn my-drop [n coll]
  (cond
    (< (count coll) n) ()
    :else (reverse (my-take (- (count coll) n) (reverse coll)))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
    (cond (and (nil? (first a-seq)) (nil? (first b-seq))) '()
          (nil? (first a-seq)) b-seq
          (nil? (first b-seq)) a-seq
          (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
          (< (first b-seq) (first a-seq)) (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
    (or  (= 1 (count a-seq)) (zero? (count a-seq))) (apply list a-seq)
    :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    false
    (or (apply > a-seq)
        (apply < a-seq))))

(defn monotonic-helper [result a-seq]
  (let [hjelp (longest-sequence (filter monotonic? (inits a-seq))) dropsize (count hjelp)]
  (cond
     (empty? hjelp) result
     :else (recur (cons hjelp result) (drop dropsize a-seq)))))

(defn split-into-monotonics [a-seq]
    (reverse (monotonic-helper '() a-seq)))

(defn permutations [a-set]
    (let [p (fn [[node & branches]]
                      (map #(cons node %) (permutations branches)))]
    (if (empty? a-set)
      (cons a-set a-set)
      (apply concat (map p (rotations a-set))))))

(defn powerset [a-set]
  (cond
    (empty? a-set) (hash-set (hash-set))
    :else (clojure.set/union (powerset (next a-set))
           (map #(conj % (first a-set)) (powerset (next a-set))))))

