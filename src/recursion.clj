(ns recursion)

(defn product [coll]
  (cond
    (empty? coll) 1
    (= 1 (count coll)) (first coll)
    :else (* (first coll)
             (product (rest coll)))))

(defn singleton? [coll]
  (cond
    (nil? coll) false
    (empty? coll) false
    :else (empty? (rest coll))))


(defn my-last [coll]
  (cond
    (nil? coll) nil
    (empty? coll) nil
    (= 1 (count coll)) (first coll)
    :else (my-last (rest coll))))


(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (= 1 (count a-seq)) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (= 1 (count a-seq)) (if (empty? (first a-seq)) nil (first a-seq))
    (> (count (first a-seq))
       (count (longest-sequence (rest a-seq)))) (first a-seq)
    :else (longest-sequence (rest a-seq))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq))  (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond
      (empty? a-seq) false
      (= elem (first a-seq)) true
      :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
  :else []))


(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) []
    (not (pred? (first a-seq))) a-seq
  :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
    (not= (count a-seq) (count b-seq)) false
    (empty? a-seq) true
    (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
    :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) []
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (= 0 k) 1
    (* n (power n (- k 1)))))

(defn fib [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (+ (fib (- n 2)) (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) []
    (cons what-to-repeat (my-repeat (- how-many-times 1) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0) []
    (cons (- up-to 1) (my-range (- up-to 1)))))

(defn tails [a-seq]
  (if (empty? a-seq) (cons [] [])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
    (if (empty? a-seq) (cons [] [])
    (cons a-seq (inits (butlast a-seq)))))

(defn rotations [a-seq]
    (let [res-seq (my-map concat (tails a-seq) (reverse (inits a-seq)))]
    (if (= 1 (count res-seq)) res-seq (rest res-seq))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (let [fe (first a-seq)
          re (rest a-seq)]
    (if (contains? freqs fe) (my-frequencies-helper freqs re)
      (my-frequencies-helper (assoc freqs fe (count (my-filter (fn [x] (= x fe)) a-seq))) re)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
    (if (empty? a-map) []
       (let [keypair (first a-map)
        k (get keypair 0)
        v (get keypair 1)]
      (concat (un-frequencies(rest a-map)) (my-repeat v k)))))

(defn my-take [n coll]
  (cond
    (>= 0 n) []
    (> n (count coll)) coll
    (= 1 n) (concat (vector (first coll)))
    :else (concat (vector (first coll)) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (= 0 n) (concat coll)
    (> n (count coll)) []
    :else (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [fcnt (int (/ (count a-seq) 2))]
    (vector (my-take fcnt a-seq) (my-drop fcnt a-seq))))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
  :else (let [as (int (first a-seq))
              bs (int (first b-seq))]
          (cond
            (< as bs) (concat (vector as) (seq-merge (rest a-seq) b-seq))
            (> as bs) (concat (vector bs) (seq-merge a-seq (rest b-seq)))
            (= as bs) (concat (vector as) (vector bs) (seq-merge (rest a-seq) (rest b-seq)))))))

(defn merge-sort [a-seq]
  (let [cnt (count a-seq)]
  (cond
    (= 0 cnt) []
    (= 1 cnt) a-seq
    :else (let [halves (halve a-seq)
                fhalf (first halves)
                lhalf (last halves)]
            (seq-merge (merge-sort fhalf) (merge-sort lhalf))))))


(defn split-into-monotonics [a-seq]
  (cond
    (empty? a-seq) []
    (= 1 (count a-seq)) a-seq
    (< (first a-seq) (get a-seq 1)) []
    (> (first a-seq) (get a-seq 1)) []
  ))

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

