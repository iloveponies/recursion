(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn not-empty? [coll]
  (not (empty? coll)))

(defn singleton? [coll]
  (let [[_ & r :as s] (seq coll)]
    (and (not-empty? s)
         (empty? r))))

(defn my-last [[f & r :as coll]]
  (cond (empty? coll) nil
        (singleton? coll) f
        :else (my-last r)))

(defn max-element [[f & r :as coll]]
  (cond (empty? coll) nil
        (singleton? coll) f
        :else (max f (max-element r))))

(defn seq-max [seq-1 seq-2]
  (let [c1 (count seq-1)
        c2 (count seq-2)]
    (if (<= c1 c2) seq-2 seq-1)))

(defn longest-sequence [[f & r :as coll]]
  (cond (empty? coll) nil
        (singleton? coll) f
        :else (seq-max f (longest-sequence r))))

(defn my-filter [pred? [f & r :as a-seq]]
  (cond (empty? a-seq) '()
        (pred? f) (cons f (my-filter pred? r))
        :else (my-filter pred? r)))

(defn sequence-contains? [elem [f & r :as a-seq]]
  (cond (empty? a-seq) false
        (= f elem) true
        :else (sequence-contains? elem r)))

(defn my-take-while [pred? [f & r :as a-seq]]
  (if (or (empty? a-seq)
          (not (pred? f)))
    '()
    (cons f (my-take-while pred? r))))

(defn my-drop-while [pred? [f & r :as a-seq]]
  (cond (empty? a-seq) '()
        (pred? f) (my-drop-while pred? r)
        :else a-seq))

(defn seq= [[f1 & r1 :as a-seq] [f2 & r2 :as b-seq]]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (= f1 f2) (seq= r1 r2)
        :else false))

(defn my-map [f [f1 & r1 :as a-seq] [f2 & r2 :as b-seq]]
  (if (or (empty? a-seq)
          (empty? b-seq))
    '()
    (cons (f f1 f2)
          (my-map f r1 r2))))

(defn power [n k]
  (if (zero? k) 1
    (* n (power n (dec k)))))

(defn fib [n]
  (if (<= n 1) n
    (+ (fib (- n 2))
       (fib (- n 1)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0) '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times)
                     what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) '()
    (cons (dec up-to)
          (my-range (dec up-to)))))

(defn tails [[f & r :as a-seq]]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn split-at-all-places [a-seq]
  (letfn [(split-iter [a-seq b-seq]
                      (if (empty? b-seq)
                        '()
                        (cons [a-seq b-seq]
                              (split-iter (conj a-seq (first b-seq))
                                          (rest b-seq)))))]
    (split-iter () (seq a-seq))))

(defn rotations [a-seq]
  (if (empty? a-seq)
      '(())
      (map #(apply concat %)
           (map reverse
                (split-at-all-places (seq a-seq))))))

(defn inc-value [m k]
  (if (get m k)
    (assoc m
      k
      (inc (get m k)))
    (assoc m k 1)))

(defn my-frequencies-helper [freqs [f & r :as a-seq]]
  (if (empty? a-seq)
    freqs
    (my-frequencies-helper (inc-value freqs f) r)))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn concater [[a q-freq] res]
  (concat res (my-repeat q-freq a)))

(defn un-frequencies-helper [iter [f & r :as a-seq]]
  (if (empty? a-seq)
    iter
    (concater f (un-frequencies-helper iter r))))

(defn un-frequencies [a-map]
  (let [freq-seq (seq a-map)]
    (un-frequencies-helper () freq-seq)))

(defn my-take [n coll]
  (letfn [(iter [i res coll]
                (if (or (= i n) (empty? coll))
                  res
                  (iter (inc i)
                        (cons (first coll) res)
                        (rest coll))))]
    (reverse (iter 0 () coll))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [n (count a-seq)
        first-n (int (/ n 2))]
    [(my-take first-n a-seq) (my-drop first-n a-seq)]))

(defn seq-merge [[f1 & r1 :as a-seq] [f2 & r2 :as b-seq]]
  (cond (empty? a-seq) b-seq
        (empty? b-seq) a-seq
        (<= f1 f2) (cons f1 (seq-merge r1 b-seq))
        :else (cons f2 (seq-merge a-seq r2))))

(defn merge-sort [[f1 & r1 :as a-seq]]
  (if (or (empty? a-seq) (empty? r1))
    (seq a-seq)
    (let [[x-seq y-seq] (halve a-seq)
          x-seq (merge-sort x-seq)
          y-seq (merge-sort y-seq)]
      (seq-merge x-seq y-seq))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn insert-at-all-positions [x a-seq]
  (let [size (count a-seq)
        split-points (range (+ 1 size))
        splits (map #(split-at % a-seq) split-points)
        joined (map #(concat (first %) [x] (second %)) splits)]
    joined))

(defn permute-seq [[f & r :as a-seq]]
  (if (empty? a-seq)
    '(())
    (mapcat #(insert-at-all-positions f %) (permute-seq r))))

(defn permutations [a-set]
  (permute-seq (seq a-set)))

(defn add-element [x coll]
  (map #(conj % x) coll))

(defn power-seq [[f & r :as a-seq]]
  (if (empty? a-seq)
    '(())
    (let [rest-power (power-seq r)]
      (concat (add-element f rest-power) rest-power))))

(defn powerset [a-set]
  (let [p-seq (power-seq (seq a-set))]
    (set (map set p-seq))))
