(ns recursion)
(require '[clojure.core.reducers :as reducers])


(defn product [coll]
  (let [mult (fn 
               ([] 1) 
               ([x y] (* x y)))]
    (reducers/fold mult coll)))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (let [f (fn 
            ([] (first a-seq))
            ([a b] (max a b)))]
    (reducers/fold f a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond 
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) 
                                (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) ()
    (pred? (first a-seq)) (cons (first a-seq)
                                (my-take-while pred? (rest a-seq)))
    :else ()))

(defn my-drop-while [pred? a-seq]
  (cond 
    (empty? a-seq) ()
    (not (pred? (first a-seq))) a-seq
    :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (or (empty? a-seq) (empty? b-seq)) false
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) ()
    :else (cons (f (first seq-1) (first seq-2))
                (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== 0 k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== 0 n) 0
    (== 1 n) 1
    :else (+ (fib (dec n)) (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
    (<= how-many-times 0) ()
    :else (cons what-to-repeat
                (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to)
    ()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) (cons a-seq ())
    :else (cons a-seq
                (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse 
       (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [prefixes (reverse (tails a-seq))
        suffixes (inits a-seq)]
    (if (empty? a-seq)
      (cons a-seq ())
      (rest (map concat
                prefixes
                suffixes)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [elem (first a-seq)
        tail (rest a-seq)]
    (cond
      (empty? a-seq) freqs
      (contains? freqs elem) (my-frequencies-helper (assoc freqs 
                                                           elem 
                                                           (+ 1 (get freqs elem)))
                                                    tail)
      :else (my-frequencies-helper (assoc freqs elem 1)
                                   tail))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [elems (keys a-map)
        amounts (vals a-map)]
    (apply concat (map (fn [amount elem] (repeat amount elem))
                       amounts
                       elems))))

(defn my-take [n coll]
  (cond
    (zero? n) ()
    (empty? coll) ()
    :else (cons (first coll)
                (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) ()
    (zero? n) coll
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [half (int (/ (count a-seq) 2))]
    [(my-take half a-seq)
     (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)
        cons-a (lazy-seq (cons a (seq-merge (rest a-seq) b-seq)))
        cons-b (lazy-seq (cons b (seq-merge a-seq (rest b-seq))))]
    (cond 
      (and (empty? a-seq) (empty? b-seq)) ()
      (empty? b-seq) cons-a
      (empty? a-seq) cons-b
      (<= a b) cons-a
      :else cons-b)))

(defn merge-sort [a-seq]
  (let [[fst snd] (halve a-seq)]
    (cond
      (empty? a-seq) ()
      (singleton? a-seq) a-seq
      :else (seq-merge (merge-sort fst) (merge-sort snd)))))

(defn split-into-monotonics [a-seq]
  (let [monotonic? (fn [b-seq] (cond
                                 (empty? b-seq) true
                                 (singleton? b-seq) true
                                 (apply >= b-seq) true
                                 (apply <= b-seq) true
                                 :else false))
        rev-inits (reverse (inits a-seq))
        longest (last (take-while monotonic? rev-inits))]
    (cond
      (empty? a-seq) ()
      :else (cons longest
                    (split-into-monotonics (drop (count longest) a-seq))))))

(defn permutations [a-set]
  (let [a-seq (seq a-set)
        fst (first a-seq)
        tail-perms (lazy-seq (permutations (rest a-seq)))]
    (cond
      (empty? a-seq) (cons () ())
      (singleton? a-seq) a-seq
      :else (map (fn [b-seq] (cons fst b-seq))
                 tail-perms))))

(defn powerset [a-set]
  [:-])

