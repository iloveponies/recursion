(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (nil? (next coll)) ((complement empty?) coll)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

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
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq)
               (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else
     '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
     '()
   (not (pred? (first a-seq)))
     (apply list a-seq)
   :else
     (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq))
     true
   (or (not= (first a-seq) (first b-seq))
       (empty? a-seq)
       (empty? b-seq))
     false
   :else
     (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (+ (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
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
    (cons (seq a-seq)
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (drop-last (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [to-inc (first a-seq)
          new-freq (if (contains? freqs to-inc)
                     (inc (get freqs to-inc))
                     1)]
      (my-frequencies-helper (conj freqs {to-inc new-freq})
                             (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [to-repeat (first a-map)]
      (concat (repeat (second to-repeat) (first to-repeat))
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (= 0 n) (empty? coll))
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (= 0 n) (empty? coll))
    coll
    (my-drop (dec n)
             (rest coll))))

(defn halve [a-seq]
  (let [middle (int (/ (count a-seq) 2))]
    [(my-take middle a-seq) (my-drop middle a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (or (empty? a-seq) (empty? b-seq))
     (concat a-seq b-seq)
   (< (first a-seq) (first b-seq))
     (cons (first a-seq)
           (seq-merge (rest a-seq)
                      b-seq))
   :else
     (cons (first b-seq)
           (seq-merge a-seq
                      (rest b-seq)))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
     a-seq
   (let [[first-half second-half] (halve a-seq)]
     (seq-merge (merge-sort first-half)
                (merge-sort second-half)))))

(defn monotonic? [a-seq]
  (if (empty? a-seq)
    true
    (or (apply <= a-seq)
        (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (if-let [monotonics (take-while monotonic? (inits a-seq))]
      (let [this-monotonic (last monotonics)]
        (cons this-monotonic
              (split-into-monotonics (drop (count this-monotonic) a-seq)))))))

(defn permute-list-build
  [partial-list set-left]
  (if (empty? set-left)
    (list partial-list)
    (let [recursive-call (fn [taken] (permute-list-build (conj partial-list taken)
                                                         (disj set-left taken)))]
      (mapcat recursive-call set-left))))

(defn permutations [a-set]
  (permute-list-build '() (set a-set)))

(defn merge-sets [singleton recursive]
  (apply conj recursive
              (set (map #(conj % (first singleton))
                        recursive))))

(defn powerset
  [a-set]
  (if (empty? a-set)
    #{#{}}
    (let [first-element (first a-set)]
      (merge-sets #{first-element}
                  (powerset (disj (set a-set) first-element))))))
