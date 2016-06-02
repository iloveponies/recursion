(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (and (not (= (first coll) false)) (empty? (rest coll)))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)(count seq-2))
     seq-1
     seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (not (rest a-seq))
      (count a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (= elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
       [])))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    a-seq
  (if (pred? (first a-seq))
    (my-drop-while pred? (rest a-seq))
     a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
    (if (or (empty? seq-1) (empty? seq-2))
      []
      (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (> 1 how-many-times)
     '()
     (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
   (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) (seq [()])
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (assoc freqs (first a-seq) (inc (get freqs (first a-seq) 0)))
                           (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) []
    (concat (concat (repeat (get (first a-map) 1)
                                  ((first a-map) 0)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
   (if (or (empty? coll) (zero? n)) []
     (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
   (if (empty? coll)
     []
     (if (> n 0)
       (my-drop (dec n) (rest coll))
       (cons (first coll) (my-drop 0 (rest coll))))))

(defn halve [a-seq]
  [(my-take (int (/ (count a-seq) 2)) a-seq)
   (my-drop (int (/ (count a-seq) 2)) a-seq)])

(defn seq-merge [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) []
        (empty? b-seq) (cons (first a-seq) (seq-merge (rest a-seq) []))
        (empty? a-seq) (cons (first b-seq) (seq-merge (rest b-seq) []))
         :else (if (< (first a-seq) (first b-seq))
                  (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
                  (cons (first b-seq) (seq-merge (rest b-seq) a-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (seq-merge (merge-sort (get (halve a-seq) 0))
               (merge-sort (get (halve a-seq) 1)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) []
  (let [init (inits a-seq)]
    (let [while-mono
         (take-while (fn [x] (or (= x (merge-sort x)) (= x (reverse (merge-sort x))))) init)]
      (cons (last while-mono) (split-into-monotonics (drop (dec (count while-mono)) a-seq)))))))

(defn permutations [a-set]
  (if (empty? a-set) '(())
    (if (singleton? a-set) a-set
      (apply concat (map (fn [x]
        (let [copy a-set]
           (map (fn [y]
             (cons x (flatten [y])))
             (permutations (remove #{x} copy))))) a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [pow (powerset (rest a-set))]
      (clojure.set/union pow (map (fn [x] (clojure.set/union #{(first a-set)} x)) pow)))))

