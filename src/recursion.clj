(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))
; (product [1 2 4])
; (product (cons 1 (cons 2 (cons 4 '())))
; (* 1 2 4 1)
; 8

(defn singleton? [coll]
  (if (empty? coll) false (empty? (rest coll))))

(defn my-last [[h & t]]
  (if (nil? t) h (my-last t)))

(defn max-element [[h & t]]
  (cond
    (nil? h) nil
    (empty? t) h
    :else (max h (max-element t))))

(defn seq-max [seq-1 seq-2]
  (let [a>b (> (count seq-1) (count seq-2))] (if a>b seq-1 seq-2)))

(defn longest-sequence [[h & t]]
  (if (empty? t)
    h
    (longest-sequence (cons (seq-max h (first t)) (rest t)))))

(defn my-filter [pred? a-seq]
  (let [[h & t] a-seq]
    (cond
      (empty? a-seq) '()
      (pred? h) (cons h (my-filter pred? t))
      :else (my-filter pred? t))))

(defn sequence-contains? [elem a-seq]
  (let [[h & t] a-seq]
    (cond (empty? a-seq) false
          (= elem h) true
          :else (sequence-contains? elem t))))

(defn my-take-while [pred? a-seq]
  (let [[h & t] a-seq]
    (cond
      (empty? a-seq) '()
      (pred? h) (cons h (my-take-while pred? t))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (let [[h & t] a-seq]
    (cond
      (empty? a-seq) '()
      (pred? h) (my-drop-while pred? t)
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (let [[h1 & t1] a-seq
        [h2 & t2] b-seq]
    (cond
      (not (= (count a-seq) (count b-seq))) false
      (and (empty? a-seq) (empty? b-seq)) true
      (= h1 h2) (seq= t1 t2)
      :else false)))

(defn my-map [f seq-1 seq-2]
  (let [[h1 & t1] seq-1
        [h2 & t2] seq-2]
    (cond
      (or (empty? seq-1) (empty? seq-2)) '()
      :else (cons (f h1 h2) (my-map f t1 t2)))))

(defn power [n k]
  (if (zero? k) 1 (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (< n 2) n
    :else (+ (fib (dec n)) (fib (- n 2)))))


(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1) '() (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) (cons '() a-seq) (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq] (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (let [not-last (fn [s] (reverse (rest (reverse s))))]
    (if (empty? a-seq) '(())
                       (not-last
                         (map
                           (fn [t] (concat (first t) (last t)))
                           (zipmap (reverse (tails a-seq)) (inits a-seq)))))))

(defn my-frequencies-helper [freqs a-seq]
  (cond
    (empty? a-seq) freqs
    :else (my-frequencies-helper
            (assoc freqs (first a-seq)
                         (inc (get freqs (first a-seq) 0)))
            (rest a-seq))))

(defn my-frequencies [a-seq] (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    [] (flatten (map (fn [[k v]] (repeat v k)) a-map))))

(defn my-take [n coll]
  (if
    (or (zero? n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if
    (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [l (int (/ (count a-seq) 2))]
    [(my-take l a-seq) (my-drop l a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [[ha & ta] a-seq
        [hb & tb] b-seq]
    (cond (and (empty? a-seq) (empty? b-seq)) '()
          (nil? ha) b-seq
          (nil? hb) a-seq
          (<= ha hb) (cons ha (seq-merge ta b-seq))
          (<= hb ha) (cons hb (seq-merge a-seq tb)))))

(defn merge-sort [a-seq]
  (let
    [sp (fn [s] (if (<= (first s) (second s)) s (reverse s)))]
    (cond
      (empty? a-seq) a-seq
      (= (count a-seq) 1) a-seq
      (= (count a-seq) 2) (sp a-seq)
      :else (seq-merge
              (merge-sort (first (halve a-seq)))
              (merge-sort (last (halve a-seq)))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

