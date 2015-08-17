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
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
      (if (= (first a-seq) elem)
      true
      (sequence-contains? elem (rest a-seq)))))


(defn my-take-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    ()
    (cons (first a-seq) (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (if (or (empty? a-seq)
          (not (pred? (first a-seq))))
    a-seq
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    true
    (if (or (empty? a-seq) (empty? b-seq))
      false
      (and (= (first a-seq) (first b-seq))
         (seq= (rest a-seq) (rest b-seq))))))


(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1)
          (empty? seq-2))
    ()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (or (= n 0)
          (= n 1))
    n
    (+ (fib (dec n))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (cons () a-seq)
    (map concat (rest (tails a-seq)) (rest (inits a-seq)))))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [frst (first a-seq)
          rst (my-frequencies-helper freqs (rest a-seq))
          cnt (get rst frst)]
      (assoc rst frst
        (if (nil? cnt)
          1
          (inc cnt))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (concat (repeat (second (first a-map)) (first (first a-map)))
            (un-frequencies (rest a-map)))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll))
    ()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
   (if (empty? coll)
     ()
     (if (zero? n)
       (cons (first coll) (my-drop 0 (rest coll)))
       (my-drop (dec n) (rest coll)))))


(defn halve [a-seq]
  (let [half (int(/ (count a-seq) 2))]
    [(my-take half a-seq) (my-drop half a-seq)]))


(defn seq-merge [a-seq b-seq]
  (cond
     (empty? a-seq) b-seq
     (empty? b-seq) a-seq
     :else (let[fa (first a-seq)
                fb (first b-seq)]
             (if (< fa fb)
             (cons fa (seq-merge (rest a-seq) b-seq))
             (cons fb (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [[frst scnd] (halve a-seq)]
     (seq-merge (merge-sort frst)
                (merge-sort scnd)))))

(defn split-into-monotonics [a-seq]
  ;Use modified monotonic? from structured_data
  (let [monotonic? #(or (empty? %) (apply >= %) (apply <= %))
        chain (last (take-while monotonic? (inits a-seq)))]
    (if (empty? a-seq)
      ()
      ;Add and drop the chain, continue with the rest
      (cons chain (split-into-monotonics (drop (count chain) a-seq))))))


(defn permutations [a-set]
  (if (empty? a-set)
    (cons () a-set)
    ;For every number in the current (sub)set
    (for [head a-set
          ;Take all but head and pass to permutatiosn
          tail (permutations (disj (set a-set) head))]
      ;Go up by constructing pairs (e.g. 5 <>, 3 <5>, 1 <3 5>)
      (cons head tail))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (clojure.set/union (powerset (rest a-set))
                       (map #(conj % (first a-set)) (powerset (rest a-set))))))

