(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (empty? coll)        false
    (empty? (rest coll)) true
    :else                false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (singleton? a-seq) (first a-seq)
    (empty? a-seq)     nil
    :else              (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq)     nil
    (singleton? a-seq) (first a-seq)
    :else              (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq)        '()
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else                 (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq)         false
    (= elem (first a-seq)) true
    :else                  (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)        '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else                 '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)              '()
    (not (pred? (first a-seq))) a-seq
    :else                       (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq))  false
    (= (first a-seq) (first b-seq))    (seq= (rest a-seq) (rest b-seq))
    :else                               false))

(defn my-map [f seq-1 seq-2]
  (if(or (empty? seq-1) (empty? seq-2))
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
    :else   (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    (cons a-seq '())
    (cons a-seq (inits (reverse (rest (reverse a-seq)))))))

(defn rotations [a-seq]
  (let [seq-1 (reverse (inits a-seq))
        seq-2 (tails a-seq)]
    (if (empty? a-seq)
      (cons '() '())
      (rest (map concat seq-2 seq-1)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [first-of-a-seq (first a-seq)
          freqs-of-first (or (freqs first-of-a-seq) 0)]
      (my-frequencies-helper
        (assoc freqs first-of-a-seq (inc freqs-of-first)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[k x]] (repeat x k)) a-map)))

(defn my-take [n coll]
  (if (or (empty? coll) (>= 0 n))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (empty? coll) (>= 0 n))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq) (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else          (let [first-of-a (first a-seq)
                         first-of-b (first b-seq)]
                     (if (<= first-of-a first-of-b)
                       (cons first-of-a (seq-merge (rest a-seq) b-seq))
                       (cons first-of-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    a-seq
    (let [[k x] (halve a-seq)]
      (seq-merge (merge-sort k) (merge-sort x)))))

(defn split-into-monotonics [a-sec]
  "nil")

(defn permutations [a-seq]
	(cond
    (empty? a-seq)     '(())
    (singleton? a-seq) (cons a-seq '())
    :else              (let [rots   (rotations a-seq)
                             helper (fn [a-rot]
                                      (map (fn [x] (cons (first a-rot) x)) (permutations (rest a-rot))))]
                         (apply concat (map helper rots)))))

(defn powerset [a-set]
   (if (empty? a-set)
       #{#{}}
       (let [without (powerset (set (rest a-set)))
             add-in (fn [x-set] (conj x-set (first a-set)))
             with (set (map add-in without))]
         (clojure.set/union without with))))

