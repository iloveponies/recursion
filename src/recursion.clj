(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

;(product [1 2 4]) => 8 (product [1 2 4]) == (* 1 (product [2 4]))
;== (* 1 2 (product [4])) == (* 1 2 4 (product '())) == (* 1 2 4 1) => 8

(defn singleton? [coll]
  (and (empty? (rest coll)) (not (empty? coll))))

(defn my-last [coll]
  (if (empty? (rest (rest coll)))
    (first (rest coll))
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (max-element (cons (max (first a-seq) (first (rest a-seq)))
                       (rest (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (or (singleton? a-seq) (empty? a-seq))
    (first a-seq)
    (longest-sequence (cons (seq-max (first a-seq) (first (rest a-seq)))
                            (rest (rest a-seq))))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
      '())))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
      a-seq)))

(defn seq= [a-seq b-seq]
  (cond
   (not (= (first a-seq) (first b-seq))) false
   (or (and (empty? a-seq) (not (empty? b-seq)))) false
   (or (and (empty? b-seq) (not (empty? a-seq)))) false
   (and (empty? a-seq) (empty? b-seq)) true
   :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))


(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (== n 0) 0
   (== n 1) 1
   :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations-helper [lnght a-seq]
  (let [rttd (cons (first (reverse a-seq)) (reverse (rest (reverse a-seq))))]
    (if (== lnght 0)
      '()
      (cons rttd (rotations-helper (- lnght 1) rttd)))))

(defn rotations [a-seq]
  (if (== (count a-seq) 0)
    '(())
    (rotations-helper (count a-seq) a-seq)))



(defn my-frequencies-helper [freqs a-seq]
  (cond
   (empty? a-seq) freqs
   (contains? freqs (first a-seq)) (my-frequencies-helper (assoc
                                    freqs
                                    (first a-seq) (+ 1 (get freqs
                                                         (first a-seq))))
                                                          (rest a-seq))
   :else (my-frequencies-helper (assoc freqs (first a-seq) 1) (rest a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (concat
     (my-repeat (get (first a-map) 1) (get (first a-map) 0))
     (un-frequencies (dissoc a-map (get (first a-map) 0))))))

(defn my-take [n coll]
  (if (or (< n 1) (< (count coll) 1))
    '()
    (cons (first coll) (my-take (- n 1) (rest coll)))))

(defn my-drop [n coll]
  (if (< n 1)
    coll
    (my-drop (- n 1) (rest coll))))

(defn halve [a-seq]
  (let [half (/ (count a-seq) 2)]
    [(my-take half a-seq) (my-drop half a-seq)]))

(defn seq-merge [a-seq b-seq]
  (if (empty? b-seq)
    a-seq
    (let [shrt (filter (fn [x] (< (first b-seq) x)) a-seq)
          delta (- (count a-seq) (count shrt))]
      (seq-merge (concat (my-take delta a-seq) (cons (first b-seq) shrt))
                 (rest b-seq)))))

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq) a-seq
   (== (count a-seq) 1) a-seq
   :else (seq-merge (merge-sort (get (halve a-seq) 0))
                    (merge-sort (get (halve a-seq) 1)))))

;from 'structured data', modified a bit
(defn monotonic? [a-seq]
  (if (< (count a-seq) 3)
    true
    (or (apply <= a-seq) (apply >= a-seq))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [lngst (longest-sequence (filter
                                   monotonic?
                                   (inits a-seq)))]
      (cons lngst (split-into-monotonics (drop (count lngst) a-seq))))))

(defn permutations-helper [a-subseq a-set]
  (let [rec (fn [elem]
              (permutations-helper
               (cons elem a-subseq)
               (disj (set a-set) elem)))]
    (if (empty? a-set)
      a-subseq
      (apply concat (map rec a-set)))))

(defn nsubs [n a-seq]
  (if (empty? a-seq)
    '()
    (cons (my-take n a-seq) (nsubs n (my-drop n a-seq)))))

(defn permutations [a-set]
  (if (empty? a-set)
    '(())
    (let [res (permutations-helper '() a-set)]
      (nsubs (count a-set) res))))

(defn apply-n [f a-seq n]
  (let [helper (fn [subseq x]
                 (if (zero? x)
                   subseq
                   (recur (apply f subseq) (dec x))))]
    (helper a-seq n)))

;  (if (== n 0)
;    a-seq
;    (apply-n f (apply f a-seq) (- n 1))))


(defn gener-ncomb ;result is set of sets
;  ([a-set 0] #{})
;  ([a-set 1]
;     (reduce (fn [iset elem] (conj iset #{elem})) #{} a-set))
;  ([a-set 2]
;     (set
;      (for [elem1 a-set
;            elem2 a-set]
;        #{elem1 elem2})))
  [a-set n]
  (loop [x 0
         cursets #{#{}}]
    (if (== x n)
      (filter (fn [st] (== (count st) n)) cursets)
      (recur (inc x)
             (set (for [elems a-set
                        newsets cursets]
                    (conj newsets elems)))))))





(defn powerset [a-set]
  (reduce (fn [pset n]
            (clojure.set/union pset (gener-ncomb a-set n)))
          #{(set a-set)} (range (count a-set))))
