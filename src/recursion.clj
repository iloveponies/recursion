(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll)) (first coll) (my-last (rest coll))))

(defn max-element [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (cond (empty? a-seq) nil
        (singleton? a-seq) (first a-seq)
        :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) [] 
   (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= elem (first a-seq)) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) []
        (not (pred? (first a-seq))) []
        :else (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) []
        (not (pred? (first a-seq))) a-seq
        :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (not (= (first a-seq) (first b-seq))) false
        :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) []
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond (= 0 k) 1
        :else (* n (power n (- k 1)))))

(defn fib [n]
  (cond (= 0 n) 0
        (= 1 n) 1
        :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond (< how-many-times 1) []
        :else (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (cond (= up-to 0) []
   :else (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (cond (empty? a-seq) [[]]
        :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate [a-seq]
  (if (empty? a-seq) a-seq (concat (rest a-seq) [(first a-seq)])))

(defn rotate2 [a-seq n]
  (cond (= n 0) [[]]
        (= n 1) [a-seq]
        :else (cons a-seq (rotate2 (rotate a-seq) (- n 1)))))

(defn rotations [a-seq]
  (rotate2 a-seq (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (cond (empty? a-seq) freqs
        (contains? freqs (first a-seq)) (my-frequencies-helper (assoc freqs (first a-seq) (+ 1 (get freqs (first a-seq)))) (rest a-seq))
        :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond (empty? a-map) []
        :else (let [x (first a-map)]
               (concat (repeat (second x) (first x)) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (< n 1) (empty? coll)) [] (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (or (< n 1) (empty? coll)) coll (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [x (int (/ (count a-seq) 2))]
       [(my-take x a-seq) (my-drop x a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) []
        (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else (cons (first b-seq) (seq-merge (rest b-seq) a-seq))))

(defn merge-sort [a-seq]
  (cond (empty? a-seq) a-seq
        (singleton? a-seq) a-seq
        :else (let [x (halve a-seq)]
               (seq-merge (merge-sort (first x)) (merge-sort (second x))))))

(defn mono? [a-seq]
  (or (>= 1 (count a-seq))
      (>= 0 (apply max (map - a-seq (rest a-seq))))
      (<= 0 (apply min (map - a-seq (rest a-seq))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) []
    (let [v (first (filter mono? (inits a-seq)))]
      (cons v (split-into-monotonics (drop (count v) a-seq))))))

(defn parts [a-seq n]
  (cond (< n 0) []
   :else (cons [(take n a-seq) (drop n a-seq)] (parts a-seq (- n 1)))))

(defn addp [x a-seq]
  (let [a (fn [[o p]] (concat o [x] p))
        b (fn [p] (map a (parts p (count p))))]
    (apply concat (map b a-seq))))

(defn permutations [a-set]
  (if (empty? a-set) [[]]
      (addp (first a-set) (permutations (rest a-set)))))

(defn adds [x a-set]
  (let [a (fn [s] (cons x s))]
    (apply concat (map a a-set) [a-set])))

(defn powerset [a-set]
  (if (empty? a-set) [[]]
      (adds (first a-set) (powerset (rest a-set)))))
