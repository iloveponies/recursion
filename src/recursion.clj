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
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

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
    (empty? a-seq)
      false
    (= (first a-seq) elem)
      true
    :else
      (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq)
      a-seq
    (pred? (first a-seq))
      (cons (first a-seq)
            (my-take-while pred? (rest a-seq)))
    :else
      ()))  ;Do I need to do something here for an empty generic sequence rather than an empty list?

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq)
      ()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      (cons (first a-seq) (rest a-seq))))  ;Need to use cons here to get () format

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) 
      true
    (or (empty? a-seq) (empty? b-seq)) ;If one of the sequences has been exhausted, but not the other one, then sequences are not equal
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2))
      ()
    :else
      (cons (f (first seq-1) (first seq-2))
            (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 
    1 
    (* n (power n (dec k)))))
     
(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else
      (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    ()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0)
    ()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [[]]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

;Not sure if it is possible to do this recursively as is - seems like would need an extra parameter to check for the base case
(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (let [rotate (fn[k] (concat (drop k a-seq) (take k a-seq)))]
      (map rotate (my-range (count a-seq))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [curr-elem (first a-seq),
          new-freqs (if (contains? freqs curr-elem)
                      (assoc freqs curr-elem (inc (get freqs curr-elem)))
                      (assoc freqs curr-elem 1))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    ()
    (let [key (first (first a-map)),
          reps (second (first a-map))]
      (concat (repeat reps key) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (empty? coll) (zero? n))
    ()
    (cons (first coll) 
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond 
    (empty? coll)
      ()
    (zero? n)
      coll
    :else
      (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [seq-len (count a-seq),
        half1-len (int (/ seq-len 2))]
    [(my-take half1-len a-seq) (my-drop half1-len a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      ()
    (empty? a-seq)
      b-seq
    (empty? b-seq)
      a-seq
    (< (first a-seq) (first b-seq))
      (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else
      (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [a-len (count a-seq)]
    (if (or (== a-len 0) (== a-len 1))
      a-seq
      (let [[half1 half2] (halve a-seq)]
        (seq-merge (merge-sort half1) (merge-sort half2)))))) 

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    []
    (if (singleton? a-seq)
      [a-seq]
      (let [prefixes (inits a-seq),
            diff (fn[seq] (map - (rest seq) seq)),
            ge0? (fn[x] (>= x 0)),
            le0? (fn[x] (<= x 0)),
            monotonic? (fn[seq] (or (every? ge0? (diff seq))
                                    (every? le0? (diff seq)))),
            mono-seq (last (take-while monotonic? prefixes)),
            remain-seq (drop (count mono-seq) a-seq)]
        (cons mono-seq (split-into-monotonics remain-seq))))))

; If monotonic, differences between elements will all be of the same sign
; (every? pos? [1 2 3 4 5])
; Could do an or with all diffs negative or all diffs positive
; use a take-while on above condition, then take the last element of this take-while
; need to store this in a let so can do a drop to get the remainder of the sequence
; (def diff (fn[seq] (map - (rest seq) seq)))


(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn permutations [a-set]
  (cond
    (empty? a-set)
      [[]]
    (singleton? a-set)
      [a-set]
    :else
      (let [rotseq (rotations a-set),
            sub-perm (fn[a-seq] (map (fn[sub-seq] (cons (first a-seq) sub-seq))
                                  (identity (permutations (rest a-seq)))))]
        (apply concat (map sub-perm rotseq)))))

(defn powerset [a-set]
  (cond 
    (empty? a-set)
      #{#{}}
    (singleton? a-set)
      #{a-set, #{}}
    :else
      (let [excl-set (powerset (set (rest a-set))),
            incl-set (map (fn[x] (set (conj x (first a-set)))) excl-set)]
        (clojure.set/union incl-set excl-set))))
    

