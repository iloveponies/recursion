(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (let [new-max (max-element (rest a-seq))
               hd (first a-seq)]
           (if (< hd new-max) new-max hd))))

(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
    (if (> len1 len2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
   :else (let [new-max (longest-sequence (rest a-seq))
               hd (first a-seq)]
           (seq-max new-max hd))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq) '()
      (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq) false
      (if (= elem (first a-seq))
        true
        (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq) '()
      (let [hd (first a-seq)
            tl (rest a-seq)]
        (if (pred? hd)
          (cons hd (my-take-while pred? tl))
          '()))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) '()
      (let [hd (first a-seq)
            tl (rest a-seq)]
        (if (pred? hd)
          (my-drop-while pred? tl)
          a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= 0 (* (count a-seq) (count b-seq))) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) '()
   :else (cons (f (first seq-1) (first seq-2))
               (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= k 0) 1
      (* n (power n (- k 1)))))

(defn fib [n]
  (if (= n 0)
    0
    (if (= n 1) 1
        (+ (fib (- n 1)) (fib (- n 2))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '()
      (cons what-to-repeat (my-repeat (dec how-many-times)
                                      what-to-repeat))))

(defn my-range [up-to]
  (if (= up-to 0) '()
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
      (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations-help [b-seq n]
  (if (= n 0) '(())
      (if (= n 1) (list b-seq)
        (let [new-seq (concat (rest b-seq) (list (first b-seq)))]
          (cons b-seq (rotations-help new-seq (dec n)))))))

(defn rotations [a-seq]
  (rotations-help (seq a-seq) (count a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
      (let [val (freqs (first a-seq))
            new-freqs (if val
                        (assoc freqs (first a-seq) (inc val))
                        (assoc freqs (first a-seq) 1))]
        (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
      (let [[key val] (first a-map)]
        (concat (repeat val key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (let [len (- (count coll) n)]
   (reverse (my-take len (reverse coll)))))

(defn halve [a-seq]
  (let [len (count a-seq)
        half (int (/ len 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a-hd (first a-seq)
               b-hd (first b-seq)]
           (if (< a-hd b-hd)
             (cons a-hd (seq-merge (rest a-seq) b-seq))
             (cons b-hd (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (= (count a-seq) 1)
          (empty? a-seq))
    a-seq
    (let [[fst-half snd-half] (halve a-seq)]
      (seq-merge (merge-sort fst-half) (merge-sort snd-half)))))

(defn all-tree [pred? a-seq]
  ())

(defn monotonical? [a-seq]
  (if (empty? a-seq) true
      (let [seq-diff (map (fn [x y] (- x y)) a-seq (rest a-seq))
            seq-1 (take-while (fn [x] (>= x 0)) seq-diff)
            seq-2 (take-while (fn [x] (<= x 0)) seq-diff)]
        (or (= (count seq-diff) (count seq-1))
            (= (count seq-diff) (count seq-2))))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) '()
      (let [lst (last (take-while monotonical? (inits a-seq)))
            len (count lst)]
        (cons lst (split-into-monotonics (drop len a-seq))))))

(defn insert-all [x a-seq]
  (if (empty? a-seq) (list (list x))
      (cons (cons x a-seq) (map (fn [y] (cons (first a-seq) y))
                                (insert-all x (rest a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set) '(())
      (apply concat (map (fn [sets] (insert-all (first a-set) sets))
                         (permutations (rest a-set))))))

(defn powerseq [a-seq]
  (if (empty? a-seq) '(())
      (let [pow (powerseq (rest a-seq))]
        (concat pow (map (fn [s] (cons (first a-seq) s)) pow)))))

(defn powerset [a-set]
  (set (map (fn [s] (set s)) (powerseq a-set))))
