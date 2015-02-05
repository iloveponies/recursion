(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))


(defn my-last [coll]
  (if (or (singleton? coll) (empty? coll))
    (first coll)
    (my-last (rest coll))))


(defn max-element [a-seq]
  (let [keep-big (fn [coll]
                   (if (< (first coll) (last coll))
                    (rest coll)
                    (pop (vec coll))))]
    (cond
     (empty? a-seq) nil
     (singleton? a-seq) (first a-seq)
      :else (max-element (keep-big a-seq)))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
         seq-1
          seq-2))

(defn longest-sequence [a-seq]
  (cond
   (empty? a-seq) nil
   (singleton? a-seq) (first a-seq)
        (= (seq-max (first a-seq) (last a-seq)) (last a-seq)) (longest-sequence (rest a-seq))
        :else (longest-sequence (butlast a-seq))))


(defn my-filter [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))


(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (not= (first a-seq) elem) (sequence-contains? elem (rest a-seq))
   :else true))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else (my-take-while pred? [])))


(defn my-drop-while [pred? a-seq]
    (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))


(defn seq= [a-seq b-seq]
  (cond
   (or (and (empty? a-seq) (not (empty? b-seq))) (and (empty? b-seq) (not (empty? a-seq)))) false
   (and (empty? a-seq) (empty? b-seq)) true
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (cond
   (or (empty? seq-1) (empty? seq-2)) []
    (and (not (empty? seq-1)) (not (empty? seq-2)))
         (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
  (= n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    []
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))


(defn my-range [up-to]
  (if (< up-to 1)
    []
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))


    (defn rotate [coll]
      (if (empty? coll)
        []
      (concat [(last coll)] (butlast coll))))



(defn rotations [a-seq]
  (let [rotate (fn [coll] (if (empty? coll)
        []
      (concat [(last coll)] (butlast coll))))]
  (cond
   (empty? a-seq) ['()]
   (and (seq? (first a-seq)) (= (count (first a-seq)) (count a-seq))) a-seq
   (seq? (first a-seq))
   (rotations (cons (rotate (first a-seq)) a-seq ))
   :else (rotations  [(rotate a-seq)]))))


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


(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq)freqs
  (not (contains? freqs (first a-seq))) (my-frequencies-helper (assoc freqs (first a-seq) (count-elem (first a-seq) a-seq)) (rest a-seq))
    :else (my-frequencies-helper freqs (rest a-seq))))


(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies-helper [a-seq a-map]
  (cond
   (empty? a-map) a-seq
    :else (un-frequencies-helper (concat a-seq (repeat (get (first a-map) 1) (get (first a-map) 0))) (rest a-map))))

(defn un-frequencies [a-map]
  (un-frequencies-helper '() a-map))

(defn my-take-helper [n coll elem]
    (if (or (= n 0) (empty? coll))
      [elem]
      (concat [elem] (my-take-helper (- n 1) (rest coll) (first coll)))))

(defn my-take [n coll]
  (my-take-helper (- n 1) (rest coll) (first coll)))

(defn my-drop [n coll]
  (if (= n 0)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [get-middle (fn [coll] (int (/ (count coll) 2)))]
    (let [a (seq (my-take (get-middle a-seq) a-seq))]
      (if (singleton? a-seq)
        (into [] (cons '()  [a-seq]))

      (into [] (cons a [(my-take (- (count a-seq) (get-middle a-seq)) (my-drop (get-middle a-seq) a-seq)
    )] ))) )))


(defn seq-merge-helper[a-seq b-seq result-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) result-seq
    (empty? a-seq) (seq-merge-helper a-seq (rest b-seq) (cons (first b-seq) result-seq))
   (empty? b-seq) (seq-merge-helper (rest a-seq) b-seq (cons (first a-seq) result-seq))
   (> (first a-seq) (first b-seq)) (seq-merge-helper (rest a-seq) b-seq (cons (first a-seq) result-seq))
    :else (seq-merge-helper a-seq (rest b-seq) (cons (first b-seq) result-seq))
   )
  )


(defn seq-merge [a-seq b-seq]
  (seq-merge-helper (reverse a-seq) (reverse b-seq) '())
  )


(defn merge-sort [a-seq]
  (if (< (count a-seq) 2)
    a-seq
    (let [left (merge-sort (first (halve a-seq)))]
    (let [right (merge-sort  (first (rest (halve a-seq))))]
      (seq-merge left right)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

