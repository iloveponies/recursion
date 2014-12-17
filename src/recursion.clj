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
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
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
   (empty? a-seq) `()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else '()))


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) `()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (not (empty? a-seq)) (empty? b-seq)) false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (empty? seq-1) '()
   (empty? seq-2) '()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (== 0 n) 0
   (== 1 n) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (>= 0 how-many-times) '()
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times)
                          what-to-repeat))))

(defn my-range [up-to]
  (cond
   (>= 0 up-to) '()
   :else (cons (- up-to 1) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (cond
   (empty? a-seq) '(())
   :else (rest (my-map concat (tails a-seq) (reverse (inits a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   :else (let [key (first a-seq)]
           (my-frequencies-helper
              (assoc freqs key (cond
                                (contains? freqs key)
                                  (inc (get freqs key))
                                :else 1))
                (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (cond
   (empty? a-map) '()
   :else (let [a-pair (first a-map)]
           (concat (repeat (first (rest a-pair)) (first a-pair))
                   (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (>= 0 n) '()
   (empty? coll) '()
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (>= 0 n) coll
   (empty? coll) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (cond
   (empty? a-seq) []
   :else (let [half-len (int (/ (count a-seq) 2))]
           (vector (my-take half-len a-seq)
                   (my-drop half-len a-seq)))))

(defn seq-merge [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) '()
   (empty? b-seq) a-seq
   (empty? a-seq) b-seq
   :else (let [first-a (first a-seq)
               first-b (first b-seq)]
           (cond
            (< first-a first-b) (cons first-a (seq-merge (rest a-seq) b-seq))
            :else (cons first-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) '()
   (singleton? a-seq) a-seq
   :else (let [[half-a half-b] (halve a-seq)]
           (seq-merge (merge-sort half-a) (merge-sort half-b)))))

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)))

(defn split-into-monotonics [a-seq]
  (cond
   (empty? a-seq) '()
   :else (let [first-monotonic
               (last (take-while monotonic?
                                 (drop 2 (reverse (inits a-seq)))))]
           (cons first-monotonic
                 (split-into-monotonics
                  (drop (count first-monotonic) a-seq))))))

(defn permutations [a-seq]
  (cond
    (empty? a-seq) '(())
    :else (mapcat (fn [e1]
              (map (fn [e2] (cons e1 e2))
                   (permutations
                    (remove (fn [x] (= x e1)) a-seq))))
            a-seq)))

(defn powerset [a-set]
  (cond
   (empty? a-set)#{#{}}
   :else  (let [elem (first a-set)
          elems-prev (rest a-set)
          powerset-prev (powerset elems-prev)]
      (clojure.set/union
        powerset-prev
        (set (map (fn [x] (conj x elem)) powerset-prev))))))


