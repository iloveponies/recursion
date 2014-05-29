(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll)) (empty? (rest coll))))

(defn my-last [coll]
  (cond
   (empty? coll) nil
   (singleton? coll) (first coll)
   :else             (my-last (rest coll))))

(defn max-element [a-seq]
  (let [max-helper (fn max-helper [max-so-far a-seq]
                     (if (empty? a-seq)
                       max-so-far
                       (max-helper (max max-so-far (first a-seq)) (rest a-seq))))]
    (max-helper (first a-seq) (rest a-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))

(defn longest-sequence [a-seq]
  (let [longest-helper (fn longest-helper [longest-so-far a-seq]
                        (if (empty? a-seq)
                          longest-so-far
                          (longest-helper (seq-max longest-so-far (first a-seq)) (rest a-seq))))]
    (longest-helper (first a-seq) (rest a-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (empty? a-seq)
    false
    (or (= (first a-seq) elem)
        (sequence-contains? elem (rest a-seq)))))

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
   (and (empty? a-seq) (empty? b-seq))      true   ; both are empty
   (not (= (empty? a-seq) (empty? b-seq)))  false  ; just one is empty
   :else (and                                      ; both are non-empty
          (= (first a-seq) (first b-seq))
          (seq= (rest a-seq) (rest b-seq)))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  "note this does not work for k < 0 !!"
  (cond
   (= k 0)   1
   (= k 1)   n
   :else     (* n (power n (- k 1)))))

(defn fib [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   :else   (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (<= how-many-times 0)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (= 0 up-to)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn inits [a-seq]
  (map (fn [i] (take i a-seq)) (range 0 (inc (count a-seq)))))

(defn tails [a-seq]
  (reverse (map reverse (inits (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '(())
    (let [r (map concat  (tails a-seq) (inits a-seq))]
      (take (dec (count r)) r))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [hd (first a-seq)
          tl (rest  a-seq)]
      (my-frequencies-helper
       (assoc freqs
         hd
         (if (contains? freqs hd)
           (inc (get freqs hd))
           1))
       tl))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k) (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (<= n 0)
    coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    [(take h a-seq) (drop h a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
   (empty? a-seq) b-seq
   (empty? b-seq) a-seq
   :else (let [a  (first a-seq)
               b  (first b-seq)]
           (if (< a b)
             (cons a (seq-merge (rest a-seq) b-seq))
             (cons b (seq-merge a-seq (rest b-seq)))))))

(defn p [msg val]
  (printf "%s: %s\n" msg val)
  val)

(defn merge-sort [a-seq]
  (cond
   (empty? a-seq)        '()
   (empty? (rest a-seq)) a-seq
   :else (let [[s1 s2] (halve a-seq)]
           (seq-merge (merge-sort s1) (merge-sort s2)))))

(defn consecutives [a-seq]
  (map vector a-seq (rest a-seq)))

(defn monotonic? [a-seq]
  (let [diffs (map #(apply - %) (consecutives a-seq))]
    (or (every? (complement neg?) diffs)
        (every? (complement pos?) diffs))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [s (last (take-while monotonic? (inits a-seq)))]
      (cons s (split-into-monotonics (drop (count s) a-seq))))))

(defn omit [a-seq n]
  "return a copy of a-seq with the n-th element omitted"
  (concat (take n a-seq) (drop (inc n) a-seq)))

(defn permutations [a-set]
  (cond
   (empty? a-set) '(())
   (empty? (rest a-set)) (list a-set)
   :else (apply
          concat
          (map
           (fn [i]
             (let [ith     (nth a-set i)
                   others  (omit a-set i)]
               (map #(cons ith %) (permutations others))))
           (range 0 (count a-set))))))

(defn powerset [a-set]
  (if (empty? a-set)
    '(())
    (let [hd  (first a-set)
          ptl (powerset (rest  a-set))]
      (concat
       ptl
       (map #(conj % hd) ptl)))))
