
(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (let [[x y] a-seq
          m (max x y)
          rest-seq (rest (rest a-seq))
          m-seq (vec (cons m (vec rest-seq)))]
      (max-element m-seq))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (let [[x y] a-seq
          m (seq-max x y)
          rest-seq (rest (rest a-seq))
          m-seq (cons m rest-seq)]
      (longest-sequence m-seq))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (if (pred? (first a-seq))
        (cons (first a-seq) (my-filter pred? (rest a-seq)))
        (my-filter pred?(rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (if (= elem (first a-seq))
    true
    (if (empty? (rest a-seq))
      false
      (sequence-contains? elem (rest a-seq)))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) []
   (not (number? (first a-seq))) []
   (not (pred? (first a-seq))) []
   :else (cons (first a-seq)
               (my-take-while pred? (rest a-seq)))))

(defn my-drop-while [pred? a-seq]
  (cond
   (empty? a-seq)
    a-seq
   (not (number? (first a-seq)))
    a-seq
   (not (pred? (first a-seq)))
    a-seq
   :else (my-drop-while pred? (rest a-seq))))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (and (empty? a-seq) (not (empty? b-seq))) false
   (and (not (empty? a-seq)) (empty? b-seq)) false
   (= (first a-seq) (first b-seq))
    (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f v w]
  (cond
   (or (empty? v)
       (empty? w))
   []
   :else (cons (f (first v) (first w))
               (my-map f (rest v) (rest w)))))

(defn power [n k]
  (cond
   (= k 0) 1
   :else (* n (power n (dec k)))))

(defn fib [n]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (cond
   (< how-many-times 1) ()
   :else (cons what-to-repeat
               (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (cond
   (< up-to 1) ()
   :else (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (cond
   (empty? a-seq) [[]]
   (empty? (rest a-seq)) (cons (vec a-seq) [[]])
   :else (cons (vec a-seq)
               (tails (rest a-seq)))))

(defn inits [a-seq]
 (map vec (reverse (map reverse (tails (vec (reverse a-seq)))))))

(defn rotations [a-seq]
  (cond
   (empty? a-seq) [[]]
   :else (reverse (rest (reverse
                  (my-map concat (tails a-seq)
                                 (inits a-seq)))))))

(defn my-frequencies-helper [freq a-seq]
  (cond
   (empty? (rest a-seq)) freq
   :else (let [elem (first a-seq)
               next-elem (first (rest a-seq))

               new-freq
               (if (= elem next-elem)
                 (inc freq)
                 freq)]

           (my-frequencies-helper new-freq
                                   (cons elem (rest (rest a-seq)))))))

(defn my-frequencies [a-seq]
  (cond
   (empty? a-seq)
     {}
   :else (let [elem (first a-seq)
               elem-filter (fn [x] (not (= x elem)))]

           (assoc (my-frequencies (my-filter elem-filter (rest a-seq)))
                  (first a-seq)
                  (my-frequencies-helper 1 a-seq)))))

(defn un-frequencies [a-map]
  (cond
   (empty? a-map) []
   :else (let [[key number] (first a-map)]
           (concat (my-repeat number key)
                   (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
   (< n 1) []
   (empty? coll) []
   :else (cons (first coll)
                 (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
   (< n 1) coll
   (empty? coll) coll
   :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [i (int (/ (count a-seq) 2))
        a (my-take i a-seq)
        b (my-drop i a-seq)]
    [a b]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
     (and (nil? a) (nil? b))
       []
     (and (not (nil? a)) (nil? b))
       (cons a (seq-merge (vec (rest a-seq))
                          (vec (rest b-seq))))
     (and (nil? a) (not (nil? b)))
       (cons b (seq-merge (vec (rest a-seq))
                          (vec (rest b-seq))))
     :else (cond
            (< a b)
              (cons a (seq-merge (vec (rest a-seq)) b-seq))
            :else
              (cons b (seq-merge a-seq (vec (rest b-seq))))))))

(defn merge-sort [a-seq]
   (if (empty? (rest a-seq))
    a-seq

    (concat (seq-merge (merge-sort (vec (first (halve a-seq))))
                     (merge-sort (vec (second (halve a-seq))))))))

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])








