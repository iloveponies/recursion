(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (empty? (rest coll)) (first coll)
   :else (my-last (rest coll) )))

(defn max-element [a-seq]
  (cond
   (empty? a-seq)    nil
   :else (apply max a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq)   nil
   (empty? (rest a-seq)) (first a-seq)
   :else (seq-max (first a-seq) (longest-sequence (rest a-seq))) ) )

(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq)   a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
   :else (my-filter pred? (rest a-seq)) ) )

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)   false
   (= (first a-seq) elem)   true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq)   ()
   (pred? (first a-seq))
     (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else () ) )


(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)   ()
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq ) )

(defn seq= [a-seq b-seq]
  (cond
   (empty? a-seq) (empty? b-seq)
   (empty? b-seq) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false ) )

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2))) ))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1))) ))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2))) ) )

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat)) ))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (- up-to 1) (my-range (- up-to 1))) ))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq))) ))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))) )

(defn rotate-helper [n lst acc]
  (if (zero? n)
    acc
    (rotate-helper
     (dec n)
     lst
     (cons (concat (drop n lst) (take n lst)) acc)) ))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (rotate-helper (count a-seq) a-seq ())))

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
    (count-elem-helper 0 elem coll))

(defn delete-all [x a-seq]
  (cond
   (empty? a-seq) ()
   (= x (first a-seq)) (delete-all x (rest a-seq))
   :else (cons (first a-seq) (delete-all x (rest a-seq))) ))

(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   :else (my-frequencies-helper
          (conj freqs {(first a-seq) (count-elem (first a-seq) a-seq)})
          (delete-all (first a-seq) a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (my-repeat (second (first a-map)) (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (cond
   (empty? coll) coll
   (<= n 0) ()
   :else (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (<= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(take half a-seq) (drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   (< (first a-seq) (first b-seq))  (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
   :else (cons (first b-seq) (seq-merge a-seq (rest b-seq))) ))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (empty? (rest a-seq)) a-seq
   :else (let [[l-seq r-seq] (halve a-seq)]
           (seq-merge (merge-sort l-seq) (merge-sort r-seq))) ))

(defn count-ascending [a-seq] ; a-seq can't be empty
  (cond
   (empty? a-seq) 0
   (empty? (rest a-seq)) 1
   (< (first a-seq) (second a-seq))  (+ 1 (count-ascending (rest a-seq)))
   :else 1 ))

(defn count-descending [a-seq] ; a-seq can't be empty
  (cond
   (empty? a-seq) 0
   (empty? (rest a-seq)) 1
   (> (first a-seq) (second a-seq))  (+ 1 (count-descending (rest a-seq)))
   :else 1 ))

(defn split-into-monotonics [a-seq]
  (cond
   (empty? a-seq) a-seq
   (empty? (rest a-seq)) (list a-seq)
   (< (first a-seq) (second a-seq))  (let [length (count-ascending a-seq)]
                                       (cons (take length a-seq)
                                             (split-into-monotonics (drop length a-seq))))
   :else (let [length (count-descending a-seq)]
           (cons (take length a-seq)
                 (split-into-monotonics (drop length a-seq))))))

(defn perm-insert [x acc a-seq]
  (let [add-last (fn [x a-seq] (reverse (cons x (reverse a-seq))))]
    (if (empty? a-seq)
      (list (add-last x acc))
      (cons (concat acc (list x) a-seq)
            (perm-insert x (add-last (first a-seq) acc) (rest a-seq))) )))

(defn perm-insert-all [x many-seq]
  (if (empty? many-seq)
    '()
    (concat (perm-insert x [] (first many-seq)) (perm-insert-all x (rest many-seq))) ))

(perm-insert-all :x '((:a) (:b :c)))

(defn permutations [a-seq]
  (if (empty? a-seq)
    '(())
    (perm-insert-all (first a-seq) (permutations (rest a-seq)))) )

(defn add-to-sets [x set-of-sets]
  (if (empty? set-of-sets)
    '()
    (set(cons (set (cons x (first set-of-sets))) (add-to-sets x (rest set-of-sets)))) ))

(defn powerset [a-set]
  (if (empty? a-set)
    #{ #{} }
    (let [powerset-of-rest (powerset (rest a-set))]
      (concat (add-to-sets (first a-set) powerset-of-rest) (powerset (rest a-set)))) ))


