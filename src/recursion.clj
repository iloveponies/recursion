(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (or (next coll) (empty? coll))
    false
    true))

(defn my-last [coll]
  (if (next coll)
    (my-last (next coll))
    (first coll)))

(defn max-element [a-seq]
  (if (next a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))
    (first a-seq)))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (reduce seq-max nil a-seq))

(defn my-filter [pred? a-seq]
  (cond (empty? a-seq) a-seq
        (pred? (first a-seq)) 
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        :else (my-filter pred? (rest a-seq))))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (= (first a-seq) elem) true
        :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
        :else '()))

(defn my-drop-while [pred? a-seq]
  (cond (empty? a-seq) '()
        (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
        :else (seq a-seq)))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
        :else false))

(defn my-map [f seq-1 seq-2]
  (cond (or (empty? seq-1) (empty? seq-2)) '()
        :else (cons (f (first seq-1) (first seq-2))
                    (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond (zero? k) 1
        :esle (* n (power n (dec k)))))

(defn fib [n]
  (cond (zero? n) 0
        (= 1 n) 1
        :else (+ (fib (dec n))
                 (fib (dec (dec n))))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond (> 1 how-many-times) '()
        :else (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (zero? up-to) 
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq) 
    (cons a-seq '())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    (list '())
    (butlast (map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [old-freq-val (or (get freqs (first a-seq)) 0)
        new-freqs (assoc freqs (first a-seq) (inc old-freq-val))]
    (cond (empty? a-seq) freqs
          :else (my-frequencies-helper new-freqs
                                       (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [kv-pair (first a-map)
        what-to-repeat (first kv-pair)
        how-many-times (first (rest kv-pair))]
    (if (empty? a-map) 
      '()
      (concat (repeat how-many-times what-to-repeat)
            (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (zero? n)
          (empty? coll)) 
    '()
    (cons (first coll)
          (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (zero? n) 
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [how-many (int (/ (count a-seq) 2))]
    [(my-take how-many a-seq) (my-drop how-many a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond (and (nil? a) (nil? b)) '()
          (nil? a) b-seq
          (nil? b) a-seq
          (< a b) (cons a (seq-merge (rest a-seq) b-seq))
          (< b a) (cons b (seq-merge a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (cond (or  (= 1 (count a-seq))
             (zero? (count a-seq))) (apply list a-seq)
        :else
        (seq-merge (merge-sort (first (halve a-seq)))
                   (merge-sort (second (halve a-seq))))))

(defn split-into-monotonics [a-seq]
  (letfn [(monotonic? [a-seq]
            (if (empty? a-seq)
              false
              (or (apply > a-seq)
                  (apply < a-seq))))
          (monotonic-helper  [result a-seq]
            (let [x (longest-sequence (filter monotonic? (inits a-seq)))
                  how-many-to-drop (count x)]
              (if (empty? x) 
                result
                (recur (cons x result) (drop how-many-to-drop a-seq)))))]
    (reverse (monotonic-helper '() a-seq))))

(defn permutations [a-set]
    (let [perm-tree (fn [[node & branches]]
                      (map #(cons node %) (permutations branches)))]
    (if (empty? a-set)
      (cons a-set a-set)
      (apply concat (map perm-tree (rotations a-set))))))


(defn powerset [a-set]
  (let [mask-length (count a-set)
        one-mask #(map read-string (map str (Integer/toString % 2)))
        set-mask-seq #(map one-mask (range 0 (Math/pow 2 %)))
        padd-it (fn [length myseq] 
                  (loop [n (count myseq)
                         result myseq]
                    (if (= n length)
                      result
                      (recur (inc n) (cons 0 result)))))
        set-mask (map #(padd-it mask-length %) (set-mask-seq mask-length))
        apply-one (fn [mask] (map #(if (= 1 %1) %2) mask a-set))
        ]
    (if (empty? a-set) 
      #{#{}}
      (set (map set (map #(remove nil? %) (map apply-one set-mask)))))))




