(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (empty? (rest coll))
       (not (empty? coll))))

(defn my-last [coll]
  (if (or (singleton? coll)
          (empty? coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq)
          (empty? a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq)
          (empty? a-seq))
    (first a-seq)
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= (first a-seq) elem) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq)
                                    (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else a-seq))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (and (empty? a-seq) (not (empty? b-seq)))
            (and (empty? b-seq) (not (empty? a-seq)))) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq)
                                              (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (= k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0)
    '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (butlast (map concat (reverse (tails a-seq)) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [elem (first a-seq)
          fil (fn [x] (= elem x))]
      (my-frequencies-helper (assoc freqs elem
                                    (count (filter fil a-seq)))
                             (filter (complement fil) a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? (keys a-map))
    '()
    (let [[elem repeats] (first a-map)]
      (concat (repeat repeats elem)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= n 0)
          (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= n 0)
          (empty? coll))
    coll
    (my-drop (dec n)
             (rest coll))))

(defn halve [a-seq]
    (let [half (int (/ (count a-seq) 2))]
      [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (<= (first a-seq) (first b-seq)) (cons (first a-seq)
                                               (seq-merge (rest a-seq)
                                                          b-seq))
        :else (cons (first b-seq) (seq-merge a-seq
                                             (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))


(defn split-into-monotonics [a-seq]
  (cond (empty? a-seq) '()
        (singleton? a-seq) (list a-seq)
        :else (let [fil (fn [x] (if (or (empty? x)
                                        (singleton? x))
                                  false
                                  (or (apply <= x)
                                      (apply >= x))))
                    sub-seq (first (filter fil (inits a-seq)))]
                (cons sub-seq
                      (split-into-monotonics (drop (count sub-seq)
                                                   a-seq))))))

(defn permutations [a-set]
  (cond (empty? a-set) '(())
        (singleton? a-set) (list a-set)
        (= (count a-set) 2) (list a-set (reverse a-set))
        :else (apply concat
                     (map (fn [x]
                            (map (fn [y] (cons (first x) y))
                                 (permutations (rest x))))
                          (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    '(())
    (let [elem (first a-set)
          others (powerset (rest a-set))]
      (clojure.set/union others
                         (map (fn [x] (cons elem x))
                              others)))))
