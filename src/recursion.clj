(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and 
    (not (empty? coll))
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (if (empty? a-seq)
      nil
      (if (empty? r)
        f
        (max f (max-element r))))))

(defn seq-max [seq-1 seq-2]
  (if (empty? seq-2)
    (if (empty? seq-1) nil seq-1)
    (if (<= (count seq-1) (count seq-2))
      seq-2
      seq-1)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (seq-max (first a-seq)
             (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (if (empty? a-seq)
      ()
      (if (pred? f)
        (cons f (my-filter pred? r))
        (my-filter pred? r)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (if (== elem (first a-seq))
      true
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (if (or (empty? a-seq)
            (not (pred? f)))
      ()
      (cons f (my-take-while pred? r)))))

(defn my-drop-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (if (empty? a-seq)
      ()
      (if (pred? f)
        (my-drop-while pred? r)
        a-seq))))

(defn seq= [a-seq b-seq]
  (let [ea (empty? a-seq)
        eb (empty? b-seq)
        fa (first a-seq)
        fb (first b-seq)]
    (if (and ea eb)
      true
      (if (or ea eb (not= fa fb))
        false
        (seq= (rest a-seq) (rest b-seq))))))

(defn my-map [f seq-1 seq-2]
  (cond
    (empty? seq-1) ()
    (empty? seq-2) ()
    :else (cons (f
                 (first seq-1)
                 (first seq-2))
                (my-map f 
                        (rest seq-1)
                        (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond 
    (zero? n) 0
    (< n 3) 1
    :else (+ (fib (- n 1))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    ()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (let [n (dec up-to)]
    (if (zero? up-to)
      ()
      (cons n (my-range n)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq
          (tails (rest a-seq)))))

(defn inits [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq
          (inits (drop-last a-seq)))))

(defn rotations-helper [n a-seq]
  (if (zero? n)
    ()
    (cons a-seq
          (rotations-helper
            (dec n)
            (cons 
              (last a-seq)
              (drop-last a-seq))))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (rotations-helper (count a-seq) a-seq)))

(defn my-frequencies-helper [freqs a-seq]
  (let [f (first a-seq)]
    (if (empty? a-seq)
      freqs
      (my-frequencies-helper
        (if (contains? freqs f)
          (assoc freqs f (+ 1 (get freqs f)))
          (assoc freqs f 1))
        (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [[k v] (first a-map)
        r (rest a-map)]
    (if (empty? a-map)
      ()
      (concat
        (my-repeat v k)
        (un-frequencies r)))))

(defn my-take [n coll]
  (if (or (zero? n) (empty? coll))
    ()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (int (/ (count a-seq) 2))]
    [(my-take n a-seq)
     (my-drop n a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [af (first a-seq)
        bf (first b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (< af bf) (cons af (seq-merge (rest a-seq) b-seq))
      :else (cons bf (seq-merge (rest b-seq) a-seq)))))

(defn merge-sort [a-seq]
  (let [size (count a-seq)
        [a, b] (halve a-seq)]
    (cond
      (empty? a-seq) ()
      (< size 2) a-seq
      :else (seq-merge
              (merge-sort a)
              (merge-sort b)))))

(defn monotonic? [a-seq]
  (or
    (apply <= a-seq)
    (apply >= a-seq)))

(defn select-first-monotonic [a-seq] 
  (if (empty? a-seq)
    ()
    (if (monotonic? (first a-seq))
      (first a-seq)
      (select-first-monotonic (rest a-seq)))))

(defn split-into-monotonics [a-seq]
  (let [f-seq (select-first-monotonic (inits a-seq))]
    (if (= (count f-seq) (count a-seq))
      [f-seq]
      (cons
        f-seq
        (split-into-monotonics (my-drop (count f-seq) a-seq))))))

(defn permutations [a-set]
  (cond
    (empty? a-set) (list ())
    (= (count a-set) 1) (list a-set)
    :else (for [i a-set
                r (permutations (remove #{i} a-set))]
            (cons i r))))

;; 3 points
;;
;; Given a set, return the powerset of that set.
;;
;; (powerset #{})      ;=> #{#{}}
;; (powerset #{1 2 4}) ;=> #{#{} #{4} #{2} #{2 4} #{1} #{1 4} #{1 2} #{1 2 4}}

;; (defn powerset [a-set]
;;   (let [helper (fn [xs]
;;                  (if (= (count xs) 1) xs
;;                    (cons xs (for [x xs] (helper (remove #{x} xs))))))]
;;     (if (empty? a-set) (list ()) (helper a-set))))


;; (defn powerset [xs]
;;   #{
;;     (if (= (count xs) 1) 
;;       #{xs}
;;       (#{xs} (for [x xs] (powerset (remove #{x} xs))))
;;       )})

(defn powerset [a-set]
  (if (empty? a-set) '(())
    (let [n (next a-set)]
      (clojure.set/union (powerset n) (map #(conj % (first a-set)) (powerset n))))))

;;                     a-set))))))
;;
;;
;; (cond
;;   (empty? a-set) (list ())
;;   (= (count a-set) 1) a-set
;;   :else (cons a-set (for [x a-set] (powerset (remove #{x} a-set))))))

