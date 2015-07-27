(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

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
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (> c1 c2)
      seq-1
      seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (cons (first a-seq)
                               (my-take-while pred? (rest a-seq)))
   :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq) '()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
     (cons (f (first seq-1) (first seq-2))
         (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (>= 0 up-to) '()
   (= 1 up-to) '(0)
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) '(())
        (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotate-n [n a-seq]
  (let [newseq (concat (rest a-seq) (list (first a-seq)))]
    (if (zero? n)
      '()
      (cons a-seq (rotate-n (dec n) newseq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotate-n (count a-seq) a-seq)))


(defn my-frequencies-helper [freqs a-seq]
  (let [firstelem (first a-seq)]
    (cond
     (empty? a-seq) freqs
     (contains? freqs firstelem)
       (my-frequencies-helper (assoc freqs firstelem
                                           (inc (freqs firstelem)))
                              (rest a-seq))
     :else
       (my-frequencies-helper (assoc freqs firstelem 1)
                              (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [firstpair (first a-map)
          firstkey (first firstpair)
          firstval (second firstpair)]
    (concat (take firstval (repeat firstkey))
            (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond (zero? n) '()
        (empty? coll) '()
        :else (cons (first coll)
                    (my-take (dec n)
                             (rest coll)))))

(defn my-drop [n coll]
    (cond
      (zero? n) coll
      (empty? coll) '()
      :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [thehalf (int (/ (count a-seq) 2))]
    (vector (my-take thehalf a-seq)
            (my-drop thehalf a-seq))))

(defn seq-merge-helper [finseq a-seq b-seq]
  (cond
   (empty? a-seq) (concat finseq b-seq)
   (empty? b-seq) (concat finseq a-seq)
   (<= (first a-seq) (first b-seq))
     (seq-merge-helper (concat finseq (list (first a-seq)))
                       (rest a-seq)
                       b-seq)
   :else
     (seq-merge-helper (concat finseq (list (first b-seq)))
                       a-seq
                       (rest b-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper '() a-seq b-seq))

(defn merge-sort [a-seq]
  (if (<= (count a-seq) 1)
    a-seq
    (let [halfseq (halve a-seq)
          seqa (first halfseq)
          seqb (second halfseq)]
      (seq-merge (merge-sort seqa)
                 (merge-sort seqb)))))

(defn split-into-monotonics-helper [finseq a-seq]
  (cond (empty? a-seq) finseq
        (= (count a-seq) 1) (concat finseq (cons a-seq '()))
        :else
        (let [my-inits (inits a-seq)
              sort-seqs (filter #(= % (merge-sort %)) my-inits)
              longest (first sort-seqs)
              longestlen (count longest)]
          (split-into-monotonics-helper (concat finseq (cons longest '()))
                                        (drop longestlen a-seq)))))

(defn split-into-monotonics [a-seq]
  (split-into-monotonics-helper '() a-seq))

(defn permutations [a-set]
  (cond
   (empty? a-set) '()
   (singleton? a-set) (first a-set)
   (= 2 (count a-set))
     (let [[a b] a-set]
       (list (list a b) (list b a)))
   :else
     (let [a (first a-set)
           b (rest a-set)]
       (list (cons a (permutations b))
             (concat (permutations b) (list a))))))

(defn powerset [a-set]
  [:-])

