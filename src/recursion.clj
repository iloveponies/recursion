(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (= (count coll) 1) true false))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll) (first coll)  (my-last (rest coll)))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))
   ))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))
   ))


(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq))
   ))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else  (sequence-contains? elem (rest a-seq))
   ))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()
   ))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (not (pred? (first a-seq))) a-seq
   :else (my-drop-while pred? (rest a-seq))
   ))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (not (= a-seq b-seq)) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) () (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1 (* n (power n (- k 1)))))

(defn fib [n]
  (if (or (= n 0) (= n 1)) n (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) () (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1) () (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(()) (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq) '(()) (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(()) (map concat (rest (tails a-seq)) (rest (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (my-frequencies-helper (assoc freqs (first a-seq)
                             (if (contains? freqs (first a-seq)) (+ 1 (get freqs (first a-seq))) 1))
    (rest a-seq))))


(defn my-frequencies [a-seq]
    (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) () (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (empty? coll) (= n 0)) () (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (= n 0)) coll (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (conj [] (my-take (int (/ (count a-seq) 2)) a-seq) (my-drop (int (/ (count a-seq) 2)) a-seq)))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   (>= (first a-seq) (first b-seq)) (cons (first b-seq) (seq-merge (rest b-seq) a-seq))
   ))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (= 1 (count a-seq)) a-seq
   :else (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))
   ))


(defn monotonics [a-seq]
  (if (or (empty? a-seq) (apply >= a-seq) (apply < a-seq)) true false))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) () (let [x (last (my-take-while monotonics (reverse (inits a-seq))))]
                            (cons x (split-into-monotonics (my-drop (count x) a-seq))))))

(defn permutations-helper [a-set b-set]
  (if (empty? a-set) b-set
    (mapcat (fn [x] (permutations-helper (clojure.set/difference a-set [x]) (cons x b-set))) a-set)))

(defn permutations [a-set]
  (if (empty? a-set) '([]) (partition (count a-set) (permutations-helper (set a-set) ()))))

(defn powerset [a-set]
  (if (empty? a-set) '(()) (let [x (powerset (rest a-set))]
                             (clojure.set/union x (set (map (fn [x] (conj x (first a-set))) x))))))


