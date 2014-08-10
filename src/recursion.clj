(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (= 1 (count coll)))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (reduce max a-seq)))

(defn seq-max [seq-1 seq-2]
  (let [count-1 (count seq-1)
        count-2 (count seq-2)]
    (if (> count-1 count-2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (if (empty? a-seq)
    nil
    (seq-max 
      (first a-seq) 
      (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons
                            (first a-seq)
                            (my-take-while pred? (rest a-seq)))
    :else (empty a-seq)))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (if (not= (count a-seq) (count b-seq)) false
    (if (and (empty? a-seq) (empty? b-seq)) true
      (let [first-a (first a-seq)
            first-b (first b-seq)]
        (cond
          (not= first-a first-b) false
          (seq= (rest a-seq) (rest b-seq)) true
          :else false)))))

(defn my-map [f seq-1 seq-2]
  (let [first-a (first seq-1)
        first-b (first seq-2)
        rest-a (rest seq-1)
        rest-b (rest seq-2)]
    (if 
      (or (empty? seq-1) (empty? seq-2)) 
      []
      (cons (f first-a first-b) (my-map f rest-a rest-b)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))


(defn my-repeat [times what]
  (cond 
    (<= times 0) []
    (= 1 times) (conj nil what)
    :else (cons what (my-repeat (dec times) what))))

(defn my-range [up-to]
  (cond
    (<= up-to 0) []
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) [[]]
    :else (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (cond
    (empty? a-seq) [[]]
    :else (conj (inits (butlast a-seq)) a-seq)))

(defn rotate [a-seq]
  (concat (rest a-seq) (conj nil (first a-seq))))


(defn rotations-helper [a-seq rotated]
  (cond
    (some #{a-seq} rotated) rotated
    :else (rotations-helper (rotate a-seq) (conj rotated a-seq))))

(defn rotations [a-seq]
  (if 
    (empty? a-seq) 
    (conj nil a-seq)
    (rotations-helper a-seq [])))


(defn my-frequencies-helper [freqs a-seq]
  (if 
    (empty? a-seq)
    freqs
    (let [value (first a-seq)
          current-value (freqs value)
          current-count (if 
                          (nil? current-value) 
                          1 
                          (inc current-value))
          new-freqs (assoc freqs value current-count)]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (if 
    (empty? a-seq)
    {}
    (my-frequencies-helper {} a-seq)))


(defn un-frequencies [a-map]
  (if 
    (empty? a-map)
    '()
    (let [value (first a-map)
          k (key value)
          v (val value)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take-helper [n coll result]
  (if
    (or (zero? n) (empty? coll))
    result
    (my-take-helper (dec n) (rest coll) (conj result (first coll)))))

(defn my-take [n coll]
  (if
    (zero? n)
    '()
    (my-take-helper n coll [])))

(defn my-drop [n coll]
  (if
    (or (empty? coll) (<= n 0))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (if 
    (empty? a-seq)
    ['() '()]
    (let [size (count a-seq)
          half (int (/ size 2))]
      [(my-take half a-seq) (my-drop half a-seq)])))

(defn seq-merge-helper [a-seq b-seq result]
  (let [empty-a (empty? a-seq)
        empty-b (empty? b-seq)]
        (cond 
          (and empty-a empty-b) result
          empty-a (seq-merge-helper 
                    a-seq 
                    (rest b-seq) 
                    (conj result (first b-seq)))
          empty-b (seq-merge-helper
                    (rest a-seq)
                    b-seq
                    (conj result (first a-seq)))
          :else (if 
                  (> (first a-seq) (first b-seq))
                  (seq-merge-helper a-seq (rest b-seq) (conj result (first b-seq)))
                  (seq-merge-helper (rest a-seq) b-seq (conj result (first a-seq)))))))


(defn seq-merge [a-seq b-seq]
    (seq-merge-helper a-seq b-seq []))

(defn merge-sort [a-seq]
  (if 
    (<= (count a-seq) 1)
    a-seq
    (let [halved (halve a-seq)
          f (halved 0)
          s (halved 1)]
      (seq-merge (merge-sort f) (merge-sort s)))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

