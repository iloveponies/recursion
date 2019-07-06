(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll) false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll) nil
    (if (empty? (rest coll)) (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (empty? (rest a-seq)) (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
    :else (my-filter pred? (rest a-seq))))
      
(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else '()))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) '()
    (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
    :else a-seq))

(defn seq= [a-seq b-seq]
  (let [rest-empty? (fn [seq] (empty? (rest seq)))]
    (cond
      (and (not (empty? a-seq)) (empty? b-seq)) false
      (and (empty? a-seq) (not (empty? b-seq))) false
      (and (empty? a-seq) (empty? b-seq)) true
      (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
      :else false)))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2) n
    (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond 
    (<= up-to 0) '()
    (< up-to 2) '(0) 
    :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
    (empty? a-seq) (vector (vector))
    :else (cons a-seq (tails (apply vector (rest a-seq))))))

(defn inits [a-seq]
  (reverse (apply vector (map reverse (tails (apply vector (reverse a-seq)))))))

;(defn seq-rev [a-seq]
;  (if (empty? a-seq) '()
;    (cons (last a-seq) (seq-rev (rest a-seq)))))


(defn rotations [a-seq]
  (cond
    (empty? a-seq) (vector(vector))
    :else (rest (my-map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [inc-key (fn [key freq-map] (assoc freq-map key (inc (freq-map key))))
        add-key (fn [key freq-map] (conj freq-map {key 1}))
        new-freq-map (fn [key freq-map] (if (contains? freq-map key) (inc-key key freq-map)
                                          (add-key key freq-map)))]
    (if (empty? a-seq) freqs
      (my-frequencies-helper (new-freq-map (first a-seq) freqs) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map) '()
    (concat (repeat (val (first a-map)) (key (first a-map))) (un-frequencies (rest a-map)))))

    (defn my-take [n coll]
      (if (or (empty? coll) (== n 0)) '()
        (cons (first coll) (my-take (dec n) (rest coll)))))

    (defn my-drop [n coll]
      (if (== n 0) coll
        (my-drop (dec n) (rest coll))))

    (defn halve [a-seq]
      (let [middle-point (int (/ (count a-seq) 2))]
        (vector (my-take middle-point a-seq) (my-drop middle-point a-seq))))

    (defn seq-merge [a-seq b-seq]
      (cond
        (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (<= (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
        :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

    (defn merge-sort [a-seq]
      (cond
        (empty? a-seq) '()
        (== (count a-seq) 1) a-seq
        :else (apply seq-merge (map merge-sort (halve a-seq)))))

    (defn split-into-monotonics [a-seq]
      (let [pred? (fn [x item] (>= item x))]
      ()))

    (defn permutations [a-set]
      [:-])

    (defn powerset [a-set]
      [:-])

