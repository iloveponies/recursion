(ns recursion)

(defn product [coll]
  (apply * coll))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (let [my-last- (fn [xs x]
                   (if (empty? xs)
                     x
                     (recur (rest xs) (first xs))))]
    (my-last- coll nil)))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (> c1 c2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [fst (first a-seq)]
    (if (empty? a-seq)
      a-seq
      (if (pred? fst)
        (cons fst (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (let [fst (first a-seq)]
    (cond
     (empty? a-seq) false
     (= fst elem) true
     :else (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [fst (first a-seq)]
    (cond
     (empty? a-seq) a-seq
     (pred? fst) (cons fst (my-take-while pred? (rest a-seq)))
     :else '())))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))   true
   (not (= (count a-seq) (count b-seq))) false
   (not (= (first a-seq) (first b-seq))) false
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons
     (f (first seq-1) (first seq-2))
     (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (let [power- (fn [n k res]
                 (if (>= 0 k)
                   res
                   (recur n (dec k) (* n res))))]
    (power- n k 1)))

(defn fib [n]
  (let [fib- (fn [m res1 res2]
               (if (>= 0 m)
                 res1
                 (recur (dec m) res2 (+ res1 res2))))]
    (fib- n 0 1)))

(defn my-repeat [n x]
  (let [my-repeat- (fn [m y res]
                     (if (>= 0 m)
                       res
                       (recur (dec m) y (cons y res))))]
    (my-repeat- n x '())))

(defn my-range [up-to]
  (let [my-range- (fn [lim n res]
                    (if (<= lim n)
                      res
                      (recur lim (inc n) (cons n res))))]
    (my-range- up-to 0 '())))

(defn parts [f a-seq]
  (let [cnts (range (+ 1 (count a-seq)))]
    (map #(f % a-seq) cnts)))

(defn inits [a-seq]
  (parts take a-seq))

(defn tails [a-seq]
  (parts drop a-seq))

(defn rotations [a-seq]
  (let [rotation (fn [n]
                   (concat (drop n a-seq) (take n a-seq)))]
    (map rotation (range (max 1 (count a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (let [freq (fn [fqs elem]
               (let [new-fq (inc (get fqs elem 0))]
                 (assoc fqs elem new-fq)))]
    (reduce freq freqs a-seq)))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [un-frequence (fn [[k n]]
                       (repeat n k))]
    (mapcat un-frequence a-map)))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (<= n 0) (empty? coll))
    (sequence coll)
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [div (int (/ (count a-seq) 2))]
    (split-at div a-seq)))

(defn seq-merge [a-seq b-seq]
  (let [f (fn [xs x]
            (let [[smaller larger] (split-with #(<= % x) xs)]
              (concat smaller (seq [x]) larger)))]
    (reduce f a-seq b-seq)))

(defn merge-sort [a-seq]
  (let [[fst snd] (halve a-seq)]
    (if (or (empty? a-seq) (singleton? a-seq))
      (sequence a-seq)
      (seq-merge (merge-sort fst) (merge-sort snd)))))

(defn split-into-monotonics [a-seq]
  (let [longest-mono (fn [xs]
                       (last
                        (take-while
                         #(or (apply < %) (apply > %))
                         (filter (complement empty?) (inits xs)))))
        mono (longest-mono a-seq)]
    (if (nil? mono)
      mono
      (cons mono (split-into-monotonics (drop (count mono) a-seq))))))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

