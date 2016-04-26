(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond (empty? coll) nil
        (empty? (rest coll)) (first coll)
        :else (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (reduce (fn [res x] (max res x)) (first a-seq) (rest a-seq))))

(defn seq-max [seq-1 seq-2]
  (loop [s1 seq-1, s2 seq-2]
    (cond (empty? s1) seq-2
          (empty? s2) seq-1
          :else (recur (rest s1) (rest s2)))))

(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (reduce (fn [res x] (seq-max res x)) (first a-seq) (rest a-seq))))

(defn my-filter [pred? a-seq]
  (reverse
    (reduce (fn [res x] (if (pred? x) (cons x res) res)) '() a-seq)))

(defn sequence-contains? [elem a-seq]
  (cond (empty? a-seq) false
        (== elem (first a-seq)) true
        :else (recur elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (reverse
    (loop [res '(), s a-seq]
      (cond (empty? s) res
            (not (pred? (first s))) res
            :else (recur (cons (first s) res) (rest s))))))

(defn my-drop-while [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (not (pred? (first a-seq))) a-seq
      (recur pred? (rest a-seq)))))

(defn seq= [a-seq b-seq]
  (cond (and (empty? a-seq) (empty? b-seq)) true
        (or (empty? a-seq) (empty? b-seq)) false
        (not (== (first a-seq) (first b-seq))) false
        :else (recur (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (loop [res '(), s1 seq-1, s2 seq-2]
    (if (or (empty? s1) (empty? s2)) (reverse res)
      (recur (cons (f (first s1) (first s2)) res) (rest s1) (rest s2)))))

(defn power [n k]
  (reduce (fn [res _] (* res n)) 1 (range k)))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else
          (loop [f-2 0, f-1 1, k 2]
            (let [fk (+ f-1 f-2)]
              (if (= k n) fk
                (recur f-1 fk (inc k)))))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (not (pos? how-many-times)) '()
    (loop [res '(), n 0]
      (if (= n how-many-times) res
        (recur (cons what-to-repeat res) (inc n))))))

(defn my-range [up-to]
  (if (not (pos? up-to)) '()
    (loop [res '(), n 0]
      (if (= n up-to) res
        (recur (cons n res) (inc n))))))

(defn tails [a-seq]
  (loop [res '(), s a-seq]
    (if (empty? s) (reverse (cons '() res))
      (recur (cons s res) (rest s)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq) '(())
    (let [src (apply vector a-seq)
          len (count src)]
      (loop [res '(), idx (range len), n 0]
        (if (= n len) (reverse res)
          (recur (cons (map (fn [i] (get src i)) idx) res)
                 (map (fn [i] (if (>= i len) (- i len) i)) (map inc idx))
                 (inc n)))))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (let [e (first a-seq)
          f (or (get freqs e) 0)]
      (recur (assoc freqs e (inc f)) (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (apply concat (map (fn [[e n]] (repeat n e)) a-map)))

(defn my-take [n coll]
  (loop [res '(), n n, s coll]
    (if (or (empty? s) (zero? n)) (reverse res)
      (recur (cons (first s) res) (dec n) (rest s)))))

(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll)) coll
    (recur (dec n) (rest coll))))

(defn halve [a-seq]
  (let [len  (count a-seq)
        left (int (/ len 2))]
    (vector (take left a-seq) (drop left a-seq))))


(defn seq-merge [a-seq b-seq]
  (loop [res '(), a a-seq, b b-seq]
    (cond 
      (and (empty? a) (empty? b)) (reverse res)

      (and (not (empty? a)) (not (empty? b)))
      (if (< (first a) (first b))
        (recur (cons (first a) res) (rest a) b)
        (recur (cons (first b) res) a (rest b)))

      (not (empty? a)) (recur (cons (first a) res) (rest a) b)

      (not (empty? b)) (recur (cons (first b) res) a (rest b)))))


(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq)) a-seq ;; already sorted
    (let [[left right] (halve a-seq)]
      (seq-merge (merge-sort left) (merge-sort right)))))

(defn sign-of [n]
  (cond (pos? n)  1
        (neg? n) -1
        :else     0))

(defn split-into-monotonics [a-seq]
  (letfn [(head-tail [s]
            (loop [head (list (first s)), sign 0, tail (rest s)]
              (if (empty? tail) [(reverse head) tail]
                (let [ff (first tail)
                      ss (sign-of (- (first head) ff))]
                  (cond (zero? sign) (recur (cons ff head) ss   (rest tail))
                        (zero? ss)   (recur (cons ff head) sign (rest tail))
                        (= sign ss)  (recur (cons ff head) sign (rest tail))
                        :else [(reverse head) tail])))))]
        (loop [res '(), s a-seq]
          (if (empty? s) (reverse res)
            (let [[head tail] (head-tail s)]
              (recur (cons head res) tail))))))

(defn two? [s]
  (and (not (empty? s)) (singleton? (rest s))))

(defn permutations [a-set]
  (cond (empty? a-set) '(())
        (singleton? a-set) (list a-set)
        (two? a-set) (list a-set (reverse a-set))
        :else
          (apply concat 
                 (map (fn [s] 
                        (map (fn [p] (cons (first s) p)) (permutations (rest s))))
                      (rotations a-set)))))



(defn powerset [a-set]
  [:-])

