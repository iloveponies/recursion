(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (not-empty coll)
    (empty? (rest coll))
    false))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (max (first a-seq))
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (<= (count seq-1) (count seq-2))
    seq-2
    seq-1))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (if (empty? (rest a-seq))
      (seq-max (first a-seq) (rest a-seq))
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
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
    ((complement pred?) (first a-seq))
      a-seq
    :else
    (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (nil? (first a-seq))
      false
    ((complement =) (first a-seq) (first b-seq))
      false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      '()
    :else
      (cons (f (first seq-1) (first seq-2))
        (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0)
      0
    (= n 1)
      1
    :else
      (+ (fib (dec (dec n)))
       (fib (dec n)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (< how-many-times 0)
      '()
    (= how-many-times 1)
      (cons what-to-repeat '())
    :else
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
    (= 0 up-to)
      '()
    (= up-to 1)
      (cons 0 '())
    :else
      (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq)
      (cons '() '())
    :else
      (cons (seq a-seq) (tails (next a-seq)))))

(defn inits [a-seq]
  (cond
    (empty? a-seq)
      (cons '() '())
    :else
      (cons (seq a-seq) (inits (butlast a-seq)))))

(defn rotations [a-seq]
  (let [c (count a-seq)
        fGetRotation (fn [whichRotation] (take c (drop (mod whichRotation c) (cycle a-seq))))
        howOften (range 0 c)]
    (if (empty? a-seq)
      (cons '() '())
      (map fGetRotation (seq howOften)))))

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

(defn my-frequencies-helper [freqs a-seq set-seq]
  "this is a huge function"
  (if (empty? set-seq)
    {}
    (assoc
      ;map
      (my-frequencies-helper freqs a-seq (rest set-seq))
      ;key
      (first set-seq)
      ;value
      (count-elem (first set-seq) a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq (set a-seq)))

(defn un-frequencies [a-map]
  (let [fstm (first (first a-map))
        sndm (second (first a-map))]
    (if (empty? a-map)
      {}
      (concat
        (un-frequencies (rest a-map))
        (repeat sndm fstm)))))

(defn my-take [n coll]
  (cond
    (<= n 0)
      (list )
    (empty? coll)
      (list )
    :else
      (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll)
      (list )
    (<= n 1)
      (rest coll)
    :else
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [hal (int (/ (count a-seq) 2))]
    (cond
      (empty? a-seq)
        []
      (= hal 0)
        (conj (conj [] ()) (list (first a-seq)))
      :else
        (conj (conj [] (my-take hal a-seq)) (my-drop hal a-seq)))))

(defn seq-merge-helper [a-seq b-seq end-seq]
  (cond
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (< (first a-seq) (first b-seq))
      (concat end-seq (list (first a-seq)) (seq-merge-helper (rest a-seq) b-seq end-seq))
    (> (first a-seq) (first b-seq))
      (concat end-seq (list (first b-seq)) (seq-merge-helper a-seq (rest b-seq) end-seq))
    :else
      (concat
        end-seq
        (list (first a-seq))
        (list (first b-seq))
        (seq-merge-helper (rest a-seq) (rest b-seq) end-seq))))

(defn seq-merge [a-seq b-seq]
  (seq-merge-helper a-seq b-seq ()))

(defn merge-sort [a-seq]
  (cond
    (<= (count a-seq) 1)
      a-seq
    :else
    (seq-merge (merge-sort (first (halve a-seq))) (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (let [iseq (reverse(inits a-seq))
        nonEmpty-iseq (rest (drop-while empty? iseq))
        plus-monotonicSeq  (last (take-while
                              (fn [x] (< (last (butlast x)) (last x)))
                              nonEmpty-iseq))
        minus-monotonicSeq  (last (take-while
                              (fn [x] (> (last (butlast x)) (last x)))
                              nonEmpty-iseq))
        monotonicSeq (or plus-monotonicSeq minus-monotonicSeq)]
    (cond
      (empty? a-seq)
        '()
      :else
      (concat (list monotonicSeq) (split-into-monotonics (drop (count monotonicSeq) a-seq))))))

(defn permutations [a-set]
  (if (empty? a-set)
    [()]
    (apply concat (map
                   (fn [b-set] (map cons (repeat (first b-set))
                                    (permutations (rest b-set))))
                   (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{()}
    (let [s (powerset (rest a-set))]
      (clojure.set/union s (map #(conj % (first a-set)) s)))))
